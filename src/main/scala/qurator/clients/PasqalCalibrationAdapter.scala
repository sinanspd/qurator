package qurator.clients

import io.circe.{HCursor, Json}
import io.circe.parser.parse
import qurator.domain.calibration._
import scala.util.Try

/** Imports provider metrics and explicit estimation assumptions. No external runtime. */
object PasqalCalibrationAdapter {
  private def optionalNumber(c: HCursor, name: String): Option[Double] =
    c.downField(name).focus.filterNot(_.isNull).map { v =>
      val n = v.asNumber.map(_.toDouble).getOrElse(throw new IllegalArgumentException(s"$name must be numeric"))
      require(n.isFinite, s"$name must be finite")
      n
    }

  def fromDeviceJson(
      raw: String,
      readoutDurationNs: Option[Long] = None,
      config: PasqalEstimationConfig = PasqalEstimationConfig()
  ): Either[String, PasqalCalibration] = Try {
    val c = parse(raw).fold(e => throw new IllegalArgumentException(e.message), identity).hcursor
    val name = c.get[String]("name").fold(e => throw new IllegalArgumentException(e.message), identity)
    val noiseJson = c.downField("noise_model").focus.filterNot(_.isNull)
      .orElse(c.downField("default_noise_model").focus.filterNot(_.isNull))
    require(noiseJson.forall(_.isObject), "Pasqal noise_model must be an object")
    val n = noiseJson.getOrElse(Json.obj()).hcursor
    val noise = PasqalNoiseCalibration(
      optionalNumber(n, "state_prep_error"), optionalNumber(n, "p_false_pos"), optionalNumber(n, "p_false_neg"),
      optionalNumber(n, "relaxation_rate"), optionalNumber(n, "dephasing_rate")
    )
    n.downField("noise_types").focus.foreach { _ =>
      val types = n.get[Vector[String]]("noise_types").fold(e => throw new IllegalArgumentException(e.message), identity)
      require(!noise.relaxationRate.exists(_ > 0) || types.contains("relaxation"), "Relaxation rate disagrees with noise_types")
      require(!noise.dephasingRate.exists(_ > 0) || types.contains("dephasing"), "Dephasing rate disagrees with noise_types")
      require(!List(noise.statePrepError, noise.falsePositive, noise.falseNegative).flatten.exists(_ > 0) || types.contains("SPAM"),
        "SPAM probabilities disagree with noise_types")
    }
    val channels = c.downField("channels").focus.filterNot(_.isNull).map { value =>
      value.asArray.getOrElse(throw new IllegalArgumentException("channels must be an array"))
    }.getOrElse(Vector.empty)
    require(channels.flatMap(_.hcursor.get[String]("basis").toOption).forall(_ == "ground-rydberg"),
      "This Pasqal mapping supports the ground-rydberg encoding only")
    val localControl = channels.exists(_.hcursor.get[String]("addressing").toOption.exists(_.equalsIgnoreCase("Local")))
    // A global drive can excite many atoms but cannot implement arbitrary independent gates.
    // Even local addressing does not prove independent capacity: that remains explicit input.
    val capacity = if (localControl) config.independentGateCapacity else 1
    val amplitudeLimits = channels.flatMap(ch => optionalNumber(ch.hcursor, "max_amp"))
    require(amplitudeLimits.forall(_ > 0), "Invalid channel amplitude limit")
    require(config.timing.rabiFrequencyRadPerUs.forall(w => amplitudeLimits.isEmpty || amplitudeLimits.exists(_ >= w)),
      "Configured operating Rabi frequency exceeds every supplied channel amplitude limit")

    val providerCoeff = optionalNumber(c, "interaction_coeff")
    require(providerCoeff.forall(_ != 0), "Invalid interaction coefficient")
    require(providerCoeff.isEmpty || config.interactionCoeffRadPerUsUm6.isEmpty || providerCoeff == config.interactionCoeffRadPerUsUm6,
      "Configured interaction coefficient conflicts with the device snapshot")
    val coefficient = providerCoeff.orElse(config.interactionCoeffRadPerUsUm6)
    val omega = config.timing.rabiFrequencyRadPerUs
    // interaction_coeff already equals C6/hbar; do not divide by hbar again.
    val blockade = for { k <- coefficient; w <- omega } yield math.exp((math.log(math.abs(k)) - math.log(w)) / 6)
    val exclusion = for {
      k <- coefficient; w <- omega; eta <- config.residualInteractionTolerance
    } yield math.exp((math.log(math.abs(k)) - math.log(w) - math.log(eta)) / 6)
    val parallelism = NeutralAtomParallelism(
      positions = config.positions,
      blockadeRadiusUm = blockade,
      addressingExclusionRadiusUm = config.addressingExclusionRadiusUm,
      maxParallelGates = capacity,
      positionUncertaintyUm = config.positionUncertaintyUm,
      interactionExclusionRadiusUm = exclusion,
      requireInteractionExclusionForAllGates = true
    )
    val expected = Vector("state_prep_error", "p_false_pos", "p_false_neg", "relaxation_rate", "dephasing_rate")
    val missing = expected.filter(k => n.downField(k).focus.forall(_.isNull))
    val extraNoise = Vector("temperature", "amp_sigma", "detuning_sigma", "depolarizing_rate", "dmm_sigma", "laser_waist")
      .filter(k => optionalNumber(n, k).exists(_ > 0))
    val warnings = Vector(
      "Pasqal analytical ranking proxy: gate-control errors and residual crosstalk are unmodeled; this is not complete circuit fidelity.",
      "Gate timing templates assume an executable implementation; the estimator does not synthesize or validate a pulse program."
    ) ++ Option.when(missing.nonEmpty)(s"Missing Pasqal noise metrics: ${missing.mkString(", ")}. Unknown values are not calibrated zeros.") ++
      Option.when(readoutDurationNs.isEmpty)("Pasqal readout duration is unavailable; supply it for circuits with measurement.") ++
      Option.when(!localControl)("Independent local control is unavailable: Pasqal gates are serialized, regardless of spatial separation.") ++
      Option.when(exclusion.isEmpty)("Interaction exclusion is unknown: supply interaction_coeff, operating Rabi frequency and interaction tolerance; independent gates are serialized.") ++
      Option.when(extraNoise.nonEmpty)(s"Additional supplied noise fields are not modeled by this approximation: ${extraNoise.mkString(", ")}.")
    val assumptions = Vector(config.timing.source) ++
      config.timing.fixedDurationNs.toVector.sortBy(_._1.toString).map { case (key, duration) => s"Timing template $key: $duration ns." } ++
      omega.map(w => s"Operating/reference Rabi frequency: $w rad/us; must apply to the configured Rydberg templates.") ++
      Option.when(config.timing.useSquarePulseRotations)(s"Resonant rectangular X/Y/SX/SXDG/RX/RY timing: ceil(1000*abs(theta)/Omega) ns plus ${config.timing.pulseOverheadNs} ns overhead for nonzero pulses.") ++
      coefficient.map(k => s"Interaction coefficient C6/hbar: $k rad*um^6/us (${if (providerCoeff.nonEmpty) "device snapshot" else "explicitly supplied"}).") ++
      config.residualInteractionTolerance.map(e => s"Configured pairwise residual interaction tolerance eta=$e; this is a scheduling assumption, not a calibrated error probability.") ++
      Vector(s"Independent gate capacity: $capacity; atom positions and addressing radius are supplied placement/control assumptions.")
    val normalizedTiming = config.timing.copy(fixedDurationNs = config.timing.fixedDurationNs.map { case (key, duration) =>
      val upper = key.name.trim.toUpperCase
      GateTimingKey(if (upper == "CROTATE") "CRZ" else upper, key.parameters.map(_.trim)) -> duration
    })
    require(normalizedTiming.fixedDurationNs.size == config.timing.fixedDurationNs.size, "Duplicate normalized timing templates")
    PasqalCalibration(name, noise, normalizedTiming, parallelism, readoutDurationNs, warnings, assumptions)
  }.toEither.left.map(_.getMessage)
}
