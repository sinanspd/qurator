package qurator

import cats.effect.IO
import io.circe.Json
import io.circe.parser.parse
import qurator.clients.PasqalCalibrationAdapter
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.util.{DecoherenceEstimator, FidelityEstimator, PasqalAnalyticalEstimator}
import weaver.SimpleIOSuite

object PasqalFidelitySuite extends SimpleIOSuite {
  // Synthetic values, not a claim about a provider's live calibration.
  private val snapshot = """{"name":"SYNTHETIC_TEST_ONLY","interaction_coeff":64,
    "channels":[{"basis":"ground-rydberg","addressing":"Local","max_amp":100,"min_duration":16}],
    "noise_model":{"state_prep_error":0.02,"p_false_pos":0.025,"p_false_neg":0.1,
      "relaxation_rate":0.01,"dephasing_rate":0.2}}"""
  private val fixed = PasqalTimingModel(
    fixedDurationNs = Map(GateTimingKey("X") -> 1000L, GateTimingKey("CZ") -> 2000L),
    rabiFrequencyRadPerUs = Some(1.0), source = "Synthetic gate timing assumptions")
  private val config = PasqalEstimationConfig(timing = fixed,
    positions = Map(0 -> AtomPosition(0, 0), 1 -> AtomPosition(5, 0)),
    residualInteractionTolerance = Some(1.0 / 64), addressingExclusionRadiusUm = Some(.5),
    independentGateCapacity = 2)
  private def right[A](v: Either[String, A]): A = v.fold(e => throw new IllegalArgumentException(e), identity)
  private def rawJson = parse(snapshot).toOption.get
  private def imported(cfg: PasqalEstimationConfig = config, raw: String = snapshot, readout: Option[Long] = Some(500L)) =
    right(PasqalCalibrationAdapter.fromDeviceJson(raw, readout, cfg))
  private def norm(c: PasqalCalibration) = FidelityEstimator.normalizeCalibration(c)
  private def score(c: PasqalCalibration, ops: List[Gate] = List(X(0), X(1), Measure(0), Measure(1))) =
    FidelityEstimator.score(Circuit(ops, ops.flatMap(gateKey(_)._2).distinct.size), norm(c))
  private def near(a: Double, b: Double) = expect(math.abs(a - b) < 1e-10)
  private def fails(body: => Any): Boolean = scala.util.Try(body).isFailure

  test("Pasqal rates map to combined T1/T2 and the analytical memory rate in seconds") {
    val n = norm(imported())
    IO.pure(near(n.t1Avg.get, 100e-6) and near(n.t2Avg.get, 1e-6 / .205) and
      near(n.readoutFidelityAvg.get, .9375) and near(n.initSurvivalPerQubit.get, .98) and
      near(DecoherenceEstimator.infidelityRate(0, n), 70000))
  }

  test("serialized runtime gates include gate and idle exposure, preparation and readout exactly once") {
    val c = imported(config.copy(independentGateCapacity = 1))
    val out = score(c)
    IO.pure(near(out.logPDecoh, -.28) and near(out.logPOps, 2 * math.log(.9375)) and
      near(out.logPInitialization, 2 * math.log(.98)) and near(out.pTotal, math.pow(.98 * .9375, 2) * math.exp(-.28)) and
      expect(out.schedule.get.operations.takeRight(2).forall(_.startNs == 2000L)) and
      expect(out.decoherence.get.perQubitExposureSec == Map(0 -> 2e-6, 1 -> 2e-6)))
  }

  test("interaction-derived parallelism changes exposure and the resulting fidelity") {
    val concurrent = score(imported())
    val serial = score(imported(config.copy(independentGateCapacity = 1)))
    IO.pure(expect(concurrent.schedule.get.peakGateParallelism == 2) and
      expect(concurrent.schedule.get.makespanNs == 1500) and near(concurrent.logPDecoh, -.14) and
      expect(concurrent.pTotal > serial.pTotal) and near(concurrent.logPCrosstalk, 0))
  }

  test("unmeasured atoms accrue noise to circuit completion") {
    val out = score(imported(config.copy(independentGateCapacity = 1)), List(X(0), X(1)))
    IO.pure(near(out.logPDecoh, -.28) and near(out.logPOps, 0))
  }

  test("no gate-error calibration is invented and partial coverage is attached to the score") {
    val c = imported()
    val n = norm(c)
    val out = score(c)
    IO.pure(expect(n.epsOptionFor(X(0)).isEmpty && n.eps1qAvg.isEmpty && n.eps2qAvg.isEmpty) and
      expect(out.decoherence.get.uncalibratedGateOperations == Vector(0, 1)) and
      expect(!out.analyticalCoverage.get.isComplete) and
      expect(out.analyticalCoverage.get.assumptions.exists(_.contains("Synthetic gate timing"))) and
      expect(out.warnings.exists(_.contains("gate-control errors"))))
  }

  test("unknown metrics remain unknown and required missing metrics prevent a numeric score") {
    val raw = """{"name":"partial","max_layout_filling":0.5,"max_atom_num":100,
      "channels":[{"min_duration":16,"max_amp":12.56}]}"""
    val c = imported(raw = raw)
    val n = norm(c)
    IO.pure(expect(n.t1Avg.isEmpty && n.t2Avg.isEmpty && n.initSurvivalPerQubit.isEmpty && n.readoutFidelityAvg.isEmpty) and
      expect(fails(score(c))) and expect(n.warnings.exists(_.contains("Missing Pasqal noise metrics"))))
  }

  test("explicit zero rates are known zero decay, not missing coherence") {
    val c = imported().copy(noise = PasqalNoiseCalibration(Some(0), Some(0), Some(0), Some(0), Some(0)))
    val out = score(c)
    IO.pure(near(out.pTotal, 1) and expect(out.decoherence.get.missingT1Qubits.isEmpty && out.decoherence.get.missingT2Qubits.isEmpty))
  }

  test("missing or overflowing decay rates cannot become a noiseless score") {
    val base = imported()
    val absent = base.copy(noise = base.noise.copy(dephasingRate = None))
    val overflow = base.copy(noise = base.noise.copy(relaxationRate = Some(1e308)))
    IO.pure(expect(fails(score(absent))) and expect(fails(score(overflow))))
  }

  test("readout metrics are required only for circuits that measure") {
    val base = imported()
    val c = base.copy(noise = base.noise.copy(falsePositive = None))
    IO.pure(expect(fails(score(c))) and expect(score(c, List(X(0))).pTotal.isFinite))
  }

  test("invalid metrics, inconsistent noise types, unsupported encoding and conflicting coefficients fail") {
    val invalid = Vector(""""relaxation_rate":-1""", """"p_false_pos":1.2""", """"dephasing_rate":"unknown""",
      """"relaxation_rate":1,"noise_types":[]""")
    IO.pure(expect(invalid.forall(x => PasqalCalibrationAdapter.fromDeviceJson(s"""{"name":"bad","noise_model":{$x}}""").isLeft)) and
      expect(PasqalCalibrationAdapter.fromDeviceJson("""{"name":"bad","channels":[{"basis":"digital"}]}""").isLeft) and
      expect(PasqalCalibrationAdapter.fromDeviceJson(snapshot, config = config.copy(interactionCoeffRadPerUsUm6 = Some(65))).isLeft))
  }

  test("timing templates are reusable across runtime circuits and preserve rotation parameters") {
    val cfg = config.copy(timing = fixed.copy(fixedDurationNs = fixed.fixedDurationNs +
      (GateTimingKey("CRZ", Vector("pi/2")) -> 2400L)))
    val n = norm(imported(cfg))
    IO.pure(expect(n.durationNsOptionFor(X(37)).contains(1000L)) and
      expect(n.durationNsOptionFor(CRZ(4, "pi/2", 9)).contains(2400L)) and
      expect(n.durationNsOptionFor(CRZ(4, "pi", 9)).isEmpty) and
      expect(fails(FidelityEstimator.score(Circuit(List(CRZ(4, "pi", 9)), 2), n))))
  }

  test("opt-in rectangular rotation timing resolves runtime angles and overhead without execution") {
    val timing = PasqalTimingModel(rabiFrequencyRadPerUs = Some(math.Pi), useSquarePulseRotations = true, pulseOverheadNs = 20)
    val n = norm(imported(config.copy(timing = timing)))
    IO.pure(expect(n.durationNsOptionFor(RX("pi/2", 0)).contains(520L)) and
      expect(n.durationNsOptionFor(RY("-(pi + pi/2)", 1)).contains(1520L)) and
      expect(n.durationNsOptionFor(RX("0", 0)).contains(0L)) and
      expect(n.durationNsOptionFor(H(0)).isEmpty && n.durationNsOptionFor(CZ(0, 1)).isEmpty) and
      expect(n.durationNsOptionFor(RX("theta", 0)).isEmpty) and
      expect(PasqalAnalyticalEstimator.angleRadians("1/0").isEmpty) and
      expect(PasqalAnalyticalEstimator.angleRadians("java.lang.Runtime.getRuntime()").isEmpty) and
      expect(PasqalAnalyticalEstimator.angleRadians("1e-3").contains(.001)))
  }

  test("hardware amplitude limits and minimum durations are not default timing estimates") {
    val c = imported(config.copy(timing = PasqalTimingModel()))
    val n = norm(c)
    val tooLarge = config.copy(timing = fixed.copy(rabiFrequencyRadPerUs = Some(101)))
    IO.pure(expect(n.durationNsOptionFor(X(0)).isEmpty) and expect(fails(score(c))) and
      expect(PasqalCalibrationAdapter.fromDeviceJson(snapshot, config = tooLarge).isLeft) and
      expect(fails(score(imported(readout = None)))))
  }

  test("blockade reach and inter-operation exclusion use different radii and no extra hbar") {
    val p = imported().parallelism
    IO.pure(near(p.blockadeRadiusUm.get, 2) and near(p.interactionExclusionRadiusUm.get, 4))
  }

  test("stronger interactions, smaller tolerance, and lower reference drive reduce parallelism") {
    val stronger = rawJson.mapObject(_.add("interaction_coeff", Json.fromInt(4096))).noSpaces
    val cases = Vector(imported(raw = stronger), imported(config.copy(residualInteractionTolerance = Some(1.0 / 4096))),
      imported(config.copy(timing = fixed.copy(rabiFrequencyRadPerUs = Some(1.0 / 64)))))
    IO.pure(expect(cases.forall(c => score(c).schedule.get.peakGateParallelism == 1)))
  }

  test("interaction exclusion covers single-atom Rydberg operations too") {
    val close = config.copy(positions = Map(0 -> AtomPosition(0, 0), 1 -> AtomPosition(3, 0)))
    IO.pure(expect(score(imported(close)).schedule.get.peakGateParallelism == 1))
  }

  test("addressing exclusion and position uncertainty independently constrain overlap") {
    val wide = config.copy(addressingExclusionRadiusUm = Some(6))
    val uncertain = config.copy(positionUncertaintyUm = .6)
    IO.pure(expect(score(imported(wide)).schedule.get.peakGateParallelism == 1) and
      expect(score(imported(uncertain)).schedule.get.peakGateParallelism == 1))
  }

  test("global or unknown control remains serial even with a requested higher capacity") {
    val global = snapshot.replace("\"Local\"", "\"Global\"")
    val unknown = rawJson.mapObject(_.remove("channels")).noSpaces
    IO.pure(expect(Vector(global, unknown).forall(raw => score(imported(raw = raw)).schedule.get.peakGateParallelism == 1)))
  }

  test("missing geometry, interaction, tolerance or operating frequency conservatively serializes") {
    val configs = Vector(config.copy(positions = Map.empty), config.copy(residualInteractionTolerance = None),
      config.copy(addressingExclusionRadiusUm = None), config.copy(timing = fixed.copy(rabiFrequencyRadPerUs = None)))
    val noK = imported(raw = rawJson.mapObject(_.remove("interaction_coeff")).noSpaces)
    IO.pure(expect((configs.map(imported(_)) :+ noK).forall(c => score(c).schedule.get.peakGateParallelism == 1)))
  }

  test("explicitly supplied coefficient works when the serialized snapshot omits it; Rydberg level alone does not") {
    val raw = rawJson.mapObject(_.remove("interaction_coeff").add("rydberg_level", Json.fromInt(60))).noSpaces
    val missing = imported(raw = raw)
    val supplied = imported(config.copy(interactionCoeffRadPerUsUm6 = Some(64)), raw)
    IO.pure(expect(missing.parallelism.blockadeRadiusUm.isEmpty) and
      near(supplied.parallelism.interactionExclusionRadiusUm.get, 4) and
      expect(score(supplied).schedule.get.peakGateParallelism == 2))
  }

  test("two-atom reach is checked separately from independent-gate exclusion") {
    val pairs = config.copy(positions = Map(0 -> AtomPosition(0, 0), 1 -> AtomPosition(1, 0),
      2 -> AtomPosition(6, 0), 3 -> AtomPosition(7, 0)))
    val c = imported(pairs)
    val out = score(c, List(CZ(0, 1), CZ(2, 3)))
    IO.pure(expect(out.schedule.get.peakGateParallelism == 2) and expect(fails(score(c, List(CZ(0, 2))))))
  }

  test("destructive readout cannot be followed by gates, repeated measurement or reset") {
    val c = imported()
    val bad = Vector(List(X(0), Measure(0), X(0)), List(Measure(0), Measure(0)), List(Reset(0)))
    IO.pure(expect(bad.forall(ops => fails(score(c, ops)))))
  }

  test("analytical estimates support larger runtime registers without simulation-size limits") {
    val ops = (0 until 32).map(X).toList ++ (0 until 32).map(Measure).toList
    val out = score(imported(config.copy(positions = Map.empty, independentGateCapacity = 1)), ops)
    val expectedLog = 32 * math.log(.98 * .9375) - .07 * 32 * 32 + math.log(.3)
    IO.pure(near(out.logPTotal, expectedLog) and expect(out.schedule.get.operations.size == 64) and
      expect(out.pTotal.isFinite && out.pTotal > 0))
  }
}
