package qurator.programs

import cats.effect.IO
import weaver.SimpleIOSuite
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.util.{DecoherenceEstimator, FidelityEstimator}

object DecoherenceEstimatorSuite extends SimpleIOSuite {
  private def approx(a: Double, b: Double, tolerance: Double = 1e-12) = expect(math.abs(a - b) <= tolerance)
  private def cal(t1: Option[Double] = Some(2.0), t2: Option[Double] = Some(1.0)): CanonicalCalibration =
    CanonicalCalibration(
      Map.empty, Map.empty, Some(0.0), Some(0.0), Map.empty, Some(1.0),
      Map.empty, Map.empty, t1, t2, Map.empty, Map.empty,
      Some(10000000L), Some(1000000L), Some(4000000L)
    )
  private val waits = List(X(0), CX(0, 1), Measure(0), Measure(1))
  private def score(ops: List[Gate], c: CanonicalCalibration) = FidelityEstimator.score(Circuit(ops, 2), c)

  test("joint rate agrees to first order with the six-state thermal relaxation channel average") {
    val c = cal()
    val dt = 1e-5
    val longitudinal = math.exp(-dt / 2.0)
    val transverse = math.exp(-dt)
    // Ground state is fixed, excited-state survival decays with T1, and the
    // four equatorial cardinal states each have survival (1 + exp(-t/T2))/2.
    val channelAverage = (1.0 + longitudinal + 4.0 * (1.0 + transverse) / 2.0) / 6.0
    val proxy = math.exp(-dt * DecoherenceEstimator.infidelityRate(0, c))
    IO.pure(approx(proxy, channelAverage, 1e-10) and expect(proxy <= 1.0))
  }

  test("relaxation-limited T2 equals twice T1 and is not counted twice") {
    val rate = DecoherenceEstimator.infidelityRate(0, cal(Some(2.0), Some(4.0)))
    IO.pure(approx(rate, 1.0 / 6.0)) // Average amplitude-damping infidelity rate = 1/(3 T1).
  }

  test("changing either T1 or T2 changes the uncovered-memory fidelity") {
    val baseline = score(waits, cal())
    val shorterT1 = score(waits, cal(Some(0.5), Some(1.0)))
    val shorterT2 = score(waits, cal(Some(2.0), Some(0.5)))
    IO.pure(expect(shorterT1.pTotal < baseline.pTotal) and expect(shorterT2.pTotal < baseline.pTotal) and
      expect(shorterT1.decoherence.get.isComplete) and expect(shorterT2.decoherence.get.isComplete))
  }

  test("calibrated gate and readout windows are excluded while dependency waits remain") {
    val out = score(waits, cal())
    val longReadout = score(waits, cal().copy(durMeasNs = Some(100000000L)))
    IO.pure(approx(out.decoherence.get.perQubitExposureSec(0), 0.0) and
      approx(out.decoherence.get.perQubitExposureSec(1), 0.001) and
      approx(out.logPDecoh, -0.001 * (1.0 / 12.0 + 1.0 / 3.0)) and
      approx(longReadout.logPDecoh, out.logPDecoh) and expect(out.decoherence.get.isComplete))
  }

  test("an explicit intrinsic-only gate error budget adds gate-time memory noise") {
    val ops = List(X(0), Measure(0))
    val c = cal().copy(eps1qAvg = Some(0.01))
    val totalError = score(ops, c)
    val intrinsicError = score(ops, c.copy(gateErrorsIncludeDecoherence = false))
    IO.pure(approx(totalError.logPDecoh, 0.0) and
      approx(intrinsicError.decoherence.get.perQubitExposureSec(0), 0.001) and
      approx(intrinsicError.logPDecoh, -0.001 * (1.0 / 12.0 + 1.0 / 3.0)) and
      approx(intrinsicError.logPOps, totalError.logPOps))
  }

  test("missing gate errors include known gate-time memory noise and mark the estimate incomplete") {
    val out = score(List(X(0), Measure(0)), cal().copy(eps1qAvg = None))
    IO.pure(approx(out.decoherence.get.perQubitExposureSec(0), 0.001) and
      expect(out.decoherence.get.uncalibratedGateOperations == Vector(0)) and
      expect(!out.decoherence.get.isComplete) and expect(out.pTotal < 1.0) and
      expect(out.schedule.get.warnings.exists(_.contains("Missing gate errors"))))
  }

  test("T1-only calibration retains amplitude damping and reports the unknown pure dephasing") {
    val out = score(waits, cal(Some(2.0), None))
    IO.pure(approx(out.logPDecoh, -0.001 / 6.0) and
      expect(out.decoherence.get.missingT2Qubits == Set(1)) and
      expect(out.decoherence.get.missingT1Qubits.isEmpty) and expect(!out.decoherence.get.isComplete))
  }

  test("T2-only calibration scores known transverse decay without fabricating T1") {
    val out = score(waits, cal(None, Some(1.0)))
    IO.pure(approx(out.logPDecoh, -0.001 / 3.0) and
      expect(out.decoherence.get.missingT1Qubits == Set(1)) and
      expect(out.decoherence.get.missingT2Qubits.isEmpty) and expect(!out.decoherence.get.isComplete))
  }

  test("absent coherence produces an explicitly partial score, not NaN or a complete estimate") {
    val out = score(waits, cal(None, None))
    IO.pure(approx(out.logPDecoh, 0.0) and expect(out.pTotal.isFinite) and
      expect(out.decoherence.get.missingT1Qubits == Set(1)) and
      expect(out.decoherence.get.missingT2Qubits == Set(1)) and expect(!out.decoherence.get.isComplete) and
      expect(out.schedule.get.warnings.exists(_.contains("incomplete"))))
  }

  test("invalid per-qubit coherence falls back to valid averages; valid per-qubit data takes priority") {
    val c = cal().copy(t1 = Map(0 -> Double.NaN, 1 -> 0.0), t2 = Map(0 -> Double.PositiveInfinity, 1 -> 0.5))
    val out = score(waits, c)
    IO.pure(expect(c.t1For(0).contains(2.0)) and expect(c.t1For(1).contains(2.0)) and
      expect(c.t2For(0).contains(1.0)) and expect(c.t2For(1).contains(0.5)) and
      approx(out.logPDecoh, -0.001 * (1.0 / 12.0 + 2.0 / 3.0)) and expect(out.decoherence.get.isComplete))
  }

  test("inconsistent T2 is capped at twice T1 and the adjustment is exposed") {
    val out = score(waits, cal(Some(0.25), Some(1.0)))
    IO.pure(approx(out.logPDecoh, -0.001 / 0.75) and
      expect(out.decoherence.get.adjustedT2Qubits == Set(1)) and expect(!out.decoherence.get.isComplete) and
      expect(out.schedule.get.warnings.exists(_.contains("T2 exceeds 2*T1"))))
  }

  test("missing and negative durations cannot silently produce complete zero-exposure estimates") {
    val out = score(waits, cal().copy(dur1qAvgNs = None))
    val negative = score(waits, cal().copy(dur1qAvgNs = Some(-1L)))
    IO.pure(expect(out.decoherence.get.missingDurationOperations == Vector(0)) and
      expect(negative.decoherence.get.missingDurationOperations == Vector(0)) and
      expect(!out.decoherence.get.isComplete) and expect(!negative.decoherence.get.isComplete))
  }

  test("unmeasured output qubits accrue memory noise through the common finish time") {
    val c = cal().copy(dur1qNs = Map("H" -> 4000000L))
    val out = score(List(X(0), H(1)), c)
    val withBarrier = score(List(X(0), H(1), NamedGate("BARRIER", Vector.empty, Vector(0, 1))), c)
    IO.pure(approx(out.decoherence.get.perQubitExposureSec(0), 0.003) and
      approx(out.decoherence.get.perQubitExposureSec(1), 0.0) and
      approx(out.logPDecoh, withBarrier.logPDecoh))
  }

  test("terminal measurement ends exposure and missing coherence is irrelevant to fully covered windows") {
    val c = cal(None, None).copy(dur1qNs = Map("H" -> 4000000L), durMeasNs = Some(1000000L))
    val out = score(List(X(0), Measure(0), H(1), Measure(1)), c)
    IO.pure(approx(out.logPDecoh, 0.0) and expect(out.decoherence.get.isComplete))
  }

  test("reset preparation does not preserve the discarded state while waiting for reset") {
    val c = cal().copy(executionModel = ExecutionModel.GlobalSerialGates, durMeasNs = Some(0L))
    val out = score(List(X(0), Reset(1), Measure(0), Measure(1)), c)
    IO.pure(approx(out.decoherence.get.perQubitExposureSec(0), 0.001) and
      approx(out.decoherence.get.perQubitExposureSec(1), 0.0))
  }

  test("neutral-atom reload prepares a new state but other live atoms keep accumulating exposure") {
    val p = NeutralAtomParallelism(reloadNs = Some(5000000L))
    val c = cal().copy(executionModel = ExecutionModel.NeutralAtom(p), durMeasNs = Some(0L))
    val out = score(List(X(1), X(0), Measure(0), X(0), Measure(0), Measure(1)), c)
    // q0 waits 1ms for the first gate; its 5ms reload is preparation. q1 waits
    // from its first gate's completion (1ms) until final measurement (8ms).
    IO.pure(approx(out.decoherence.get.perQubitExposureSec(0), 0.001) and
      approx(out.decoherence.get.perQubitExposureSec(1), 0.007))
  }

  test("post-measurement waits on a reused nondestructive qubit still carry memory noise") {
    val c = cal().copy(dur1qNs = Map("H" -> 30000000L))
    val out = score(List(X(0), Measure(0), H(1), CX(0, 1), Measure(0), Measure(1)), c)
    IO.pure(approx(out.decoherence.get.perQubitExposureSec(0), 0.019) and
      approx(out.decoherence.get.perQubitExposureSec(1), 0.0))
  }

  test("a virtual gate between destructive measurement and reload does not resurrect memory exposure") {
    val c = cal().copy(executionModel = ExecutionModel.NeutralAtom(NeutralAtomParallelism(reloadNs = Some(5000000L))),
      dur1qNs = Map("RZ" -> 0L))
    val out = score(List(X(0), Measure(0), RZ("pi", 0), X(0)), c)
    IO.pure(expect(out.schedule.get.makespanNs == 17000000L) and
      approx(out.decoherence.get.perQubitExposureSec(0), 0.0))
  }

  test("empty circuits and zero exposure stay finite even for extremely short coherence times") {
    val c = cal(Some(java.lang.Double.MIN_VALUE), Some(java.lang.Double.MIN_VALUE))
    val empty = score(Nil, c)
    val covered = score(List(X(0), Measure(0)), c)
    val exposed = score(waits, c)
    IO.pure(approx(empty.pTotal, 1.0) and approx(covered.pTotal, 1.0) and
      expect(exposed.logPDecoh == Double.NegativeInfinity) and approx(exposed.pTotal, 0.0))
  }
}
