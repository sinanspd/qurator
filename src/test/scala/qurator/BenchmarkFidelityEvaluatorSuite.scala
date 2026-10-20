package qurator.testbed.fidelity

import cats.effect.IO
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.testbed.BenchmarkDeviceRegistry
import qurator.util.FidelityEstimator
import weaver.SimpleIOSuite

object BenchmarkFidelityEvaluatorSuite extends SimpleIOSuite {
  private val ideal = CanonicalCalibration(
    Map.empty, Map.empty, Some(0), Some(0), Map.empty, Some(1),
    Map.empty, Map.empty, None, None, Map.empty, Map.empty, Some(0L), Some(0L), Some(0L))
  private def simulator(samples: Int = 40000, strict: Boolean = false) =
    new PauliTraceEvaluator(PauliTraceEvaluator.Settings(samples, 123L, strict,
      PauliTraceEvaluator.PauliProbability))
  private def near(actual: Double, expected: Double, tolerance: Double = 0.008) =
    expect(math.abs(actual - expected) < tolerance)

  test("legacy adapter preserves the original score and log score exactly") {
    val circuit = Circuit(List(H(0), CX(0, 1), Measure(0), Measure(1)), 2)
    val cal = ideal.copy(eps1qAvg = Some(0.02), eps2qAvg = Some(0.1), readoutFidelityAvg = Some(0.9))
    val old = FidelityEstimator.score(circuit, cal)
    val result = BenchmarkFidelityEvaluator.Legacy.evaluate(circuit, cal)
    IO.pure(expect(result.score == old.pTotal) and expect(result.logScore == old.logPTotal))
  }

  test("Clifford transfers propagate X/Z through H and a directed CX exactly") {
    val h = PauliTransfer.build(H(0))
    val cx = PauliTransfer.build(CX(1, 7))
    IO.pure(expect(!h.approximated && !cx.approximated) and
      expect(h.rows(1).outcomes.toList == List(2)) and
      expect(h.rows(2).outcomes.toList == List(1)) and
      expect(cx.rows(1).outcomes.toList == List(5)) and
      expect(cx.rows(8).outcomes.toList == List(10)))
  }

  test("non-Clifford transfer implements squared Pauli coefficients and reports approximation") {
    val t = PauliTransfer.build(T(0))
    val result = simulator(64).evaluate(Circuit(List(T(0), Measure(0)), 1), ideal)
    IO.pure(expect(t.rows(1).outcomes.toList == List(1, 3)) and
      near(t.rows(1).cumulative(0), 0.5, 1e-12) and
      expect(result.approximatedGates == 1) and expect(result.score == 1))
  }

  test("native benchmark gate units match the existing IonQ lowering convention") {
    def same(a: Gate, b: Gate): Boolean = {
      val left = PauliTransfer.build(a).rows
      val right = PauliTransfer.build(b).rows
      left.zip(right).forall { case (x, y) =>
        x.outcomes.toList == y.outcomes.toList &&
          x.cumulative.zip(y.cumulative).forall { case (p, q) => math.abs(p - q) < 1e-12 }
      }
    }
    IO.pure(expect(same(NamedGate("gpi2", Vector("0.25"), Vector(0)), RY("pi/2", 0))) and
      expect(same(NamedGate("gpi", Vector("0"), Vector(0)), X(0))) and
      expect(same(NamedGate("zz", Vector("pi/2"), Vector(0, 1)),
        NamedGate("rzz", Vector("pi/2"), Vector(0, 1)))))
  }

  test("a wide noiseless non-Clifford circuit uses frames without an exponential state") {
    val width = 500
    val gates = (0 until width).toList.flatMap(q => List(H(q), T(q))) ++
      (1 until width).toList.map(q => CX(q - 1, q))
    val result = simulator(16).evaluate(Circuit(gates, width), ideal)
    IO.pure(expect(result.score == 1) and expect(result.approximatedGates == width))
  }

  test("two faults can cancel: score agrees with the analytic Pauli channel, not no-fault product") {
    val p = 0.4
    val result = simulator().evaluate(Circuit(List(Id(0), Id(0)), 1), ideal.copy(eps1qAvg = Some(p)))
    IO.pure(near(result.score, (1 - p) * (1 - p) + p * p / 3) and
      expect(result.score > (1 - p) * (1 - p) + 0.03))
  }

  test("average gate infidelity is converted to nonidentity Pauli probability") {
    val sim = new PauliTraceEvaluator(PauliTraceEvaluator.Settings(samples = 40000))
    val one = sim.evaluate(Circuit(List(Id(0)), 1), ideal.copy(eps1qAvg = Some(0.2)))
    val two = sim.evaluate(Circuit(List(CX(0, 1)), 2), ideal.copy(eps2qAvg = Some(0.2)))
    IO.pure(near(one.score, 0.7) and near(two.score, 0.75))
  }

  test("physical edge calibration overrides averages and sparse IDs do not allocate device width") {
    val cal = ideal.copy(eps2q = Map(((7, 1000000), "CX") -> 0.3))
    val result = simulator().evaluate(Circuit(List(CX(1000000, 7)), 1000001), cal)
    IO.pure(near(result.score, 0.7))
  }

  test("ASAP idle T1/T2 agrees with a twirled thermal channel and excludes active gate time") {
    val cal = ideal.copy(t1Avg = Some(1e-6), t2Avg = Some(2e-6),
      dur1qNs = Map("X" -> 1000L))
    // q0 idles for 1000 ns while q1 runs X, before their noiseless CX.
    val result = simulator().evaluate(Circuit(List(Id(0), X(1), CX(0, 1)), 2), cal)
    val expected = (1 + 2 * math.exp(-0.5) + math.exp(-1)) / 4
    IO.pure(near(result.score, expected))
  }

  test("global serial timing exposes the other qubit, independently of the ranking scheduler") {
    val ops = Circuit(List(X(0), X(1)), 2)
    val cal = ideal.copy(t1Avg = Some(1e-6), t2Avg = Some(2e-6), dur1qAvgNs = Some(1000L))
    val parallel = simulator().evaluate(ops, cal)
    val serial = simulator().evaluate(ops, cal.copy(executionModel = ExecutionModel.GlobalSerialGates))
    val single = (1 + 2 * math.exp(-0.5) + math.exp(-1)) / 4
    IO.pure(expect(parallel.score == 1) and near(serial.score, single * single))
  }

  test("readout probabilities are independent checkpoints; reset discards incoming errors") {
    val readout = simulator().evaluate(Circuit(List(Measure(0), Measure(1)), 2),
      ideal.copy(readoutFidelity = Map(0 -> 0.8, 1 -> 0.9)))
    val reset = simulator(64).evaluate(Circuit(List(X(0), Reset(0), Measure(0)), 1),
      ideal.copy(eps1qAvg = Some(1)))
    IO.pure(near(readout.score, 0.72) and expect(reset.score == 1))
  }

  test("a serial gate profile still allows independent simultaneous readout") {
    val cal = ideal.copy(durMeasNs = Some(1000L), t1Avg = Some(1e-6), t2Avg = Some(2e-6),
      executionModel = ExecutionModel.GlobalSerialGates)
    val result = simulator(256).evaluate(Circuit(List(Measure(0), Measure(1)), 2), cal)
    IO.pure(expect(result.score == 1))
  }

  test("same inputs and seed are reproducible across calls, with bounded Wilson uncertainty") {
    val sim = simulator(1024)
    val circuit = Circuit(List(H(0), T(0), CX(0, 1), Measure(0), Measure(1)), 2)
    val cal = ideal.copy(eps1qAvg = Some(0.03), eps2qAvg = Some(0.1))
    val first = sim.evaluate(circuit, cal)
    val second = sim.evaluate(circuit, cal)
    val (lo, hi) = first.confidence95.get
    IO.pure(expect(first == second) and expect(lo <= first.score && hi >= first.score && lo >= 0 && hi <= 1))
  }

  test("zero surviving samples retain a positive upper bound and a resolution warning") {
    val result = simulator(128).evaluate(Circuit(List(Id(0)), 1), ideal.copy(eps1qAvg = Some(1)))
    IO.pure(expect(result.score == 0) and expect(result.confidence95.get._2 > 0) and
      expect(result.warnings.exists(_.contains("sampling resolution"))))
  }

  test("missing calibration is visible and strict mode rejects it") {
    val circuit = Circuit(List(X(0)), 1)
    val cal = ideal.copy(eps1qAvg = None, dur1qAvgNs = None)
    val result = simulator(64).evaluate(circuit, cal)
    IO(simulator(64, strict = true).evaluate(circuit, cal)).attempt.map { strict =>
      expect(result.warnings.exists(_.contains("Missing 1-qubit gate error"))) and
        expect(result.warnings.exists(_.contains("temporal noise disabled"))) and expect(strict.isLeft)
    }
  }

  test("all enabled hard-coded profiles cover a gate trace and disclose imputed benchmark values") {
    val ops = Circuit(List(H(0), CX(0, 1), Measure(0), Measure(1)), 2)
    val results = BenchmarkDeviceRegistry.defaultDevices.map { device =>
      val raw = BenchmarkDeviceRegistry.defaultCalibrations(device.platformId)
      device.platformId -> simulator(64).evaluate(ops, FidelityEstimator.normalizeCalibration(raw))
    }.toMap
    IO.pure(expect(results.values.forall(r => r.score >= 0 && r.score <= 1)) and
      expect(results.values.forall(r => !r.warnings.exists(_.startsWith("Missing")))) and
      expect(results("braket-ionq-forte-1").warnings.exists(_.contains("Benchmark assumption"))) and
      expect(results("braket-iqm-garnet").warnings.exists(_.contains("Benchmark assumptions"))))
  }

  test("completed IQM and IonQ benchmark profiles pass strict calibration coverage") {
    val ops = Circuit(List(H(0), CX(0, 1), Measure(0), Measure(1)), 2)
    val calibrations = List("braket-iqm-garnet", "braket-ionq-forte-1").map { id =>
      FidelityEstimator.normalizeCalibration(BenchmarkDeviceRegistry.defaultCalibrations(id))
    }
    val results = calibrations.map(simulator(64, strict = true).evaluate(ops, _))
    IO.pure(expect(results.forall(_.score.isFinite)) and
      expect(calibrations.head.dur1qAvgNs.contains(20L)) and
      expect(calibrations.head.dur2qAvgNs.contains(40L)) and
      expect(calibrations.head.durMeasNs.contains(1000L)) and
      near(calibrations(1).eps1qAvg.get, 0.0003, 1e-12))
  }

  test("unknown gates, symbolic angles and gate-after-measurement fail explicitly") {
    val bad = List(
      Circuit(List(NamedGate("mystery", Vector.empty, Vector(0))), 1),
      Circuit(List(RZ("theta", 0)), 1),
      Circuit(List(Measure(0), H(0)), 1))
    IO.pure(expect(bad.forall(c => scala.util.Try(simulator(8).evaluate(c, ideal)).isFailure)))
  }

  test("Pauli settings reject nonpositive sample counts") {
    IO.pure(expect(scala.util.Try(PauliTraceEvaluator.Settings(samples = 0)).isFailure) and
      expect(scala.util.Try(PauliTraceEvaluator.Settings(samples = -1)).isFailure))
  }
}
