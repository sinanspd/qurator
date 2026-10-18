package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import scala.util.Try

/** Arithmetic-only timing and validation for the Pasqal ranking proxy. */
object PasqalAnalyticalEstimator {
  def durationNs(op: Gate, model: PasqalTimingModel): Option[Long] = {
    model.fixedDurationNs.get(gateTimingKey(op)).orElse {
      if (!model.useSquarePulseRotations) None
      else {
        val angle = op match {
          case X(_) | Y(_) => Some(math.Pi)
          case SX(_) | SXDG(_) => Some(math.Pi / 2)
          case RX(theta, _) => angleRadians(theta)
          case RY(theta, _) => angleRadians(theta)
          case _ => None // No invented implementation for RZ, H, or entangling gates.
        }
        for {
          theta <- angle
          omega <- model.rabiFrequencyRadPerUs
          ns = math.ceil(math.abs(theta) / omega * 1000)
          if ns.isFinite && ns >= 0 && ns < Long.MaxValue.toDouble
          total <- Try(if (ns == 0) 0L else Math.addExact(ns.toLong, model.pulseOverheadNs)).toOption
        } yield total
      }
    }
  }

  /** A small bounded numeric grammar, not executable expressions or a scripting runtime. */
  private[qurator] def angleRadians(raw: String): Option[Double] = Try {
    require(raw.length <= 256, "Angle expression is too long")
    val input = raw.filterNot(_.isWhitespace)
    var index = 0
    def take(c: Char): Boolean = {
      val found = index < input.length && input(index) == c
      if (found) index += 1
      found
    }
    def expression(): Double = {
      var value = term()
      var more = true
      while (more) {
        if (take('+')) value += term()
        else if (take('-')) value -= term()
        else more = false
      }
      value
    }
    def term(): Double = {
      var value = atom()
      var more = true
      while (more) {
        if (take('*')) value *= atom()
        else if (take('/')) value /= atom()
        else more = false
      }
      value
    }
    def atom(): Double = {
      if (take('+')) atom()
      else if (take('-')) -atom()
      else if (take('(')) {
        val value = expression()
        require(take(')'), "Unclosed angle expression")
        value
      } else if (input.startsWith("pi", index)) {
        index += 2
        math.Pi
      } else {
        val start = index
        while (index < input.length && (input(index).isDigit || input(index) == '.')) index += 1
        if (index < input.length && (input(index) == 'e' || input(index) == 'E')) {
          index += 1
          if (!take('+')) take('-')
          while (index < input.length && input(index).isDigit) index += 1
        }
        input.substring(start, index).toDouble
      }
    }
    val result = expression()
    require(index == input.length && result.isFinite, "Invalid or unresolved angle")
    result
  }.toOption

  def validate(ops: List[Gate], cal: CanonicalCalibration): Unit = {
    val physical = ops.filter(op => gateKey(op)._2.nonEmpty && gateKey(op)._1 != "BARRIER")
    if (physical.nonEmpty) {
      require(cal.initSurvivalPerQubit.nonEmpty && cal.memoryInfidelityRatePerSecond.exists(r => r.isFinite && r >= 0),
        "Pasqal analytical score unavailable: supply state_prep_error, relaxation_rate and dephasing_rate with a finite mapped rate (explicit zero is valid).")
      require(!physical.exists(_.isInstanceOf[Measure]) || cal.readoutFidelityAvg.nonEmpty,
        "Pasqal analytical score unavailable: both readout false-positive and false-negative probabilities are required.")
      val missing = physical.collect { case op if cal.durationNsOptionFor(op).isEmpty => gateTimingKey(op) }
      require(missing.isEmpty,
        s"Pasqal analytical score unavailable: supply timing templates or resolve numeric rotation angles; missing durations: ${missing.distinct.mkString(", ")}. Readout needs explicit timing too.")
      require(!physical.exists(op => gateKey(op)._2.size > 2 || op.isInstanceOf[Reset]),
        "Pasqal analytical gate scoring needs one-/two-atom templates; reset/reload and collective analog operations are unsupported.")
    }
    val firstMeasurement = ops.indexWhere(_.isInstanceOf[Measure])
    val terminal = if (firstMeasurement < 0) Nil else ops.drop(firstMeasurement)
    require(terminal.forall(op => op.isInstanceOf[Measure] || Set("BARRIER", "GPHASE").contains(gateKey(op)._1)),
      "Pasqal measurement is destructive; post-measurement gates require an explicit reload model.")
    val measured = terminal.collect { case Measure(q) => q }
    require(measured.distinct == measured, "Pasqal atoms cannot be measured repeatedly without reloading.")
    cal.executionModel match {
      case ExecutionModel.NeutralAtom(p) =>
        for {
          radius <- p.blockadeRadiusUm
          op <- physical if !op.isInstanceOf[Measure]
          qs = gateKey(op)._2 if qs.size == 2 && qs.forall(p.positions.contains)
        } require(p.positions(qs(0)).distanceTo(p.positions(qs(1))) + 2 * p.positionUncertaintyUm <= radius,
          s"Pasqal operation ${gateTimingKey(op)} is outside the configured blockade reach; no transport implementation is supplied.")
      case _ => ()
    }
  }

  def coverage(cal: CanonicalCalibration): AnalyticalFidelityCoverage = AnalyticalFidelityCoverage(
    modeledContributions = Vector("preparation", "state-averaged readout", "relaxation and pure dephasing during gates and idle time"),
    unmodeledEffects = Vector("gate-control errors", "residual crosstalk beyond exclusion constraints", "correlated noise"),
    assumptions = cal.analyticalAssumptions ++ Vector(
      "Independent-atom, state-averaged weak-noise product approximation; no state evolution is computed.",
      "Exposure begins at circuit start and ends at measurement start, or at circuit completion for unmeasured atoms.",
      "The existing width heuristic c(W) is applied separately from the physical noise model."
    )
  )
}
