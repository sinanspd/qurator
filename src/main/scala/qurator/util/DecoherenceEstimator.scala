package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import scala.collection.mutable

/** State-averaged, weak-noise memory proxy; not a statevector/channel simulation.
  *
  * For Markovian relaxation and dephasing, F_avg(t) =
  * (3 + exp(-t/T1) + 2 exp(-t/T2)) / 6. Its initial infidelity rate is
  * 1/(6 T1) + 1/(3 T2). Exponentiating that rate gives an additive log score
  * independent of how idle intervals are split by the scheduler.
  */
object DecoherenceEstimator {
  private case class Rates(t1: Option[Double], t2: Option[Double]) {
    val adjustedT2: Boolean = t1.exists(a => t2.exists(b => b / 2.0 > a))
    private val relaxation = t1.fold(0.0)(1.0 / _)
    // T2 already includes half the longitudinal relaxation rate. T1 alone
    // therefore implies transverse decay too; missing T2 is not zero decay.
    private val transverse = t2.fold(relaxation / 2.0)(1.0 / _).max(relaxation / 2.0)
    val infidelityPerSecond: Double = relaxation / 6.0 + transverse / 3.0
  }

    def infidelityRate(q: Int, cal: CanonicalCalibration): Double =
    cal.memoryInfidelityRatePerSecond.filter(r => r.isFinite && r >= 0)
      .getOrElse(Rates(cal.t1For(q), cal.t2For(q)).infidelityPerSecond)

  def estimate(ops: List[Gate], timed: Vector[TimedGate], cal: CanonicalCalibration): DecoherenceEstimate = {
    val gates = ops.toVector
    val horizon = timed.iterator.map(_.endNs).maxOption.getOrElse(0L)
    val used = timed.iterator.flatMap(_.qubits).toSet
    val exposureNs = mutable.Map.from(used.iterator.map(_ -> 0L))
    val previousEnd = mutable.Map.empty[Int, Long].withDefaultValue(0L)
    val measured = mutable.Set.empty[Int]
    val reloadsAtoms = cal.executionModel.isInstanceOf[ExecutionModel.NeutralAtom]

    def add(q: Int, durationNs: Long): Unit =
      if (durationNs > 0L) exposureNs(q) = Math.addExact(exposureNs(q), durationNs)

    timed.foreach { interval =>
      val gate = gates(interval.index)
      val (name, _) = gateKey(gate)
      // Barriers do not prepare/destroy a state or cover any physical exposure.
      if (name != "BARRIER") interval.qubits.foreach { q =>
        // A virtual instruction cannot recreate a destructively measured atom.
        // Keep it marked for preparation until the pulse that actually reloads it.
        val awaitingReload = reloadsAtoms && measured(q) && !interval.isMeasurement &&
          interval.endNs == interval.gateStartNs
        if (!awaitingReload) {
          val prepares = gate.isInstanceOf[Reset] ||
            (reloadsAtoms && measured(q) && !interval.isMeasurement)
          if (!prepares) add(q, interval.gateStartNs - previousEnd(q))

          // Calibrated gate errors and readout fidelities cover their own windows.
          // With no gate error, include known memory noise but flag missing other errors.
          if (!interval.isMeasurement && (!cal.gateErrorsIncludeDecoherence || cal.epsOptionFor(gate).isEmpty))
            add(q, interval.endNs - interval.gateStartNs)

          previousEnd(q) = interval.endNs
          if (interval.isMeasurement) measured += q else measured -= q
        }
      }
    }
    // Unmeasured output qubits must survive until the circuit finishes. Terminal
    // measured qubits instead stop at readout start (readout error covers readout).
    used.diff(measured.toSet).foreach(q => add(q, horizon - previousEnd(q)))

    val exposures = exposureNs.iterator.map { case (q, ns) => q -> (ns.toDouble / 1e9) }.toMap
    val exposed = exposures.filter(_._2 > 0.0)
    val rates = exposed.keysIterator.map(q => q -> Rates(cal.t1For(q), cal.t2For(q))).toMap
    val explicitRate = cal.memoryInfidelityRatePerSecond.exists(r => r.isFinite && r >= 0)
    val missingT1 = rates.collect { case (q, r) if r.t1.isEmpty && !explicitRate => q }.toSet
    val missingT2 = rates.collect { case (q, r) if r.t2.isEmpty && !explicitRate => q }.toSet
    val adjusted = rates.collect { case (q, r) if r.adjustedT2 => q }.toSet
    val missingDurations = gates.indices.filter(i => gateKey(gates(i))._2.nonEmpty && cal.durationNsOptionFor(gates(i)).isEmpty).toVector
    val missingErrors = gates.indices.filter(i => cal.epsOptionFor(gates(i)).isEmpty).toVector
    val warnings = Vector.newBuilder[String]
    if (missingT1.nonEmpty)
      warnings += s"Missing positive T1 for exposed qubits ${missingT1.toVector.sorted.mkString(",")}; only known transverse decay is scored. Decoherence estimate is incomplete."
    if (missingT2.nonEmpty)
      warnings += s"Missing positive T2 for exposed qubits ${missingT2.toVector.sorted.mkString(",")}; only relaxation-implied transverse decay is scored. Decoherence estimate is incomplete."
    if (adjusted.nonEmpty)
      warnings += s"T2 exceeds 2*T1 for exposed qubits ${adjusted.toVector.sorted.mkString(",")}; T2 is capped at 2*T1 for the Markovian model. Calibration requires review."
    if (missingDurations.nonEmpty)
      warnings += s"Missing durations at operation indices ${missingDurations.mkString(",")}; decoherence exposure is incomplete."
    if (missingErrors.nonEmpty)
      warnings += s"Missing gate errors at operation indices ${missingErrors.mkString(",")}; known memory noise is included during these gates, but other gate errors are unmodeled."

    DecoherenceEstimate(
      logP = -exposed.iterator.map { case (q, seconds) => seconds * infidelityRate(q, cal) }.sum,
      perQubitExposureSec = exposures,
      missingT1Qubits = missingT1,
      missingT2Qubits = missingT2,
      adjustedT2Qubits = adjusted,
      missingDurationOperations = missingDurations,
      uncalibratedGateOperations = missingErrors,
      warnings = warnings.result()
    )
  }
}
