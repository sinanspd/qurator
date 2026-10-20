package qurator.testbed

import cats.effect.IO
import qurator.domain.circuit._
import qurator.domain.cutting.CutLocation
import qurator.testbed.QuantumPolicyBenchmark._
import qurator.util.{CuttingStrategies, HardwareAwareCuttingPlanner}
import weaver.SimpleIOSuite

object FullQuantumSchedulerBenchmarkSuite extends SimpleIOSuite {
  private val devices = BenchmarkDeviceRegistry.defaultDevices.filter(_.platform == "IBM").take(2).toVector
  private val initial = initialQueues(devices, 42L)
  private val circuit = Circuit(List(X(0), Measure(0)), 1, "one")

  test("full scheduler merges independent quantum inputs without classical parent tasks") {
    FullQuantumSchedulerBenchmark.run(Vector(circuit, circuit, circuit), devices, initial, 0.0).map { result =>
      expect(result.originalInputs == 3) and expect(result.rows.size == 3) and
        expect(result.providerJobs == 1) and expect(result.mergedJobs == 1) and expect(result.cutInputs == 0) and
        expect(result.rows.map(_.queueNs).distinct.size == 1) and
        expect(result.rows.map(_.candidate.compiled).distinct.size == 1)
    }
  }

  test("full scheduler executes cut fragments and preserves their explicit proxy rows") {
    val input = Circuit(List(X(0), X(1), Measure(0), Measure(1)), 2, "cut-input")
    val cutter: CuttingStrategies.CuttingStrategy[IO] = request => {
      val decision = HardwareAwareCuttingPlanner.decisionFromSubcircuits(
        request, "test-split", List(circuit.copy(name = "left"), circuit.copy(name = "right")), Nil)
      IO.pure(decision.copy(selected = decision.selected.copy(
        cutLocations = List(CutLocation(0, "WIRE", List(0), 4.0)))))
    }
    FullQuantumSchedulerBenchmark.run(Vector(input), devices, initial, 1.0, Some(cutter)).map { result =>
      expect(result.originalInputs == 1) and expect(result.rows.size == 2) and
        expect(result.providerJobs == 2) and expect(result.batchedJobs == 2) and
        expect(result.cutInputs == 1) and expect(result.mergedJobs == 0) and
        expect(result.rows.forall(_.candidate.compiled.name.contains("_topology_")))
    }
  }

  test("native hardware-aware cutting executes an input that cannot fit any device") {
    val narrow = devices.map(_.copy(qubits = 1))
    val calibrations = devices.map { d =>
      val raw = BenchmarkDeviceRegistry.defaultCalibrations(d.platformId)
        .asInstanceOf[qurator.domain.calibration.IBMCalibration]
      d.platformId -> raw.copy(qubits = List(0), edges = Nil,
        t1Seconds = raw.t1Seconds.filter(_._1 == 0), t2Seconds = raw.t2Seconds.filter(_._1 == 0))
    }.toMap
    val bell = Circuit(List(H(0), CX(0, 1), Measure(0), Measure(1)), 2, "oversized-bell")
    FullQuantumSchedulerBenchmark.run(Vector(bell), narrow, initial, 0.9, calibrations = calibrations).map { result =>
      expect(result.originalInputs == 1) and expect(result.cutInputs == 1) and
        expect(result.rows.size >= 2) and expect(result.providerJobs == result.rows.size) and
        expect(result.rows.forall(r =>
          r.candidate.compiled.remainingGates.flatMap(qurator.domain.calibration.gateKey(_)._2).distinct.size <= 1))
    }
  }
}
