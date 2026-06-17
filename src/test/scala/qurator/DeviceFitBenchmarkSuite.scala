package qurator

import cats.effect.IO
import org.typelevel.log4cats.noop.NoOpLogger
import qurator.domain.Task.QuantumTask
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.device.Device
import qurator.domain._
import qurator.testbed.HaqaMapper.DeviceTopology
import qurator.testbed.DeviceFitBenchmark
import qurator.util.{CircuitProcessConverter, Qasm3Parser}
import qurator.util.HaloCircuitMerger
import qurator.util.HaloCircuitMerger.ProcessInstruction
import qurator.util.HaloCircuitMerger.ProcessInstruction.Op
import qurator.util.HaloCircuitMerger.VirtualQubitRef.Helper
import weaver.SimpleIOSuite

import java.nio.charset.StandardCharsets

object DeviceFitBenchmarkSuite extends SimpleIOSuite {

  test("partitionBySimilarDepth groups by tolerance and orders circuits by qubit count") {
    def prepared(name: String, qubits: Int, depth: Int): DeviceFitBenchmark.PreparedCircuit =
      DeviceFitBenchmark.PreparedCircuit(
        name = name,
        circuit = Circuit(List.fill(depth)(X(0)), qubits, name),
        process = DeviceFitBenchmark.processFromCircuit(Circuit(List(X(0)), qubits, name)),
        qubits = qubits,
        depth = depth
      )

    val partitions =
      DeviceFitBenchmark.partitionBySimilarDepth(
        Vector(
          prepared("wide", qubits = 5, depth = 110),
          prepared("small", qubits = 2, depth = 100),
          prepared("next", qubits = 3, depth = 116)
        ),
        tolerance = 0.15
      )

    IO.pure(
      expect(partitions.size == 2) and
      expect(partitions.head.circuits.map(_.name) == Vector("small", "wide")) and
      expect(partitions(1).circuits.map(_.name) == Vector("next"))
    )
  }

  test("processFromCircuit converts QASM wires into live-interval dynamic refs") {
    val circuit =
      Circuit(
        List(
          H(0),
          CX(0, 1),
          CRZ(1, "pi/4", 0),
          Measure(1),
          Reset(0)
        ),
        qubits = 2,
        name = "sample"
      )

    val process = DeviceFitBenchmark.processFromCircuit(circuit)

    IO.pure(
      expect(process.dataQubits == 0) and
      expect(process.helperQubits == 2) and
      expect(
        process.instructions == Vector(
          Op("h", refs = Vector(Helper(0))),
          Op("cx", refs = Vector(Helper(0), Helper(1))),
          Op("crz", params = Vector("pi/4"), refs = Vector(Helper(1), Helper(0))),
          ProcessInstruction.Measure(Helper(1)),
          ProcessInstruction.Release(Vector(1)),
          ProcessInstruction.Reset(Helper(0)),
          ProcessInstruction.Release(Vector(0))
        )
      )
    )
  }

  test("processFromCircuit releases a dynamic ref after reset before later reuse") {
    val circuit =
      Circuit(
        List(
          H(0),
          Reset(0),
          X(0),
          Measure(0)
        ),
        qubits = 1,
        name = "reset-segment"
      )

    val process = DeviceFitBenchmark.processFromCircuit(circuit)

    IO.pure(
      expect(
        process.instructions == Vector(
          Op("h", refs = Vector(Helper(0))),
          ProcessInstruction.Reset(Helper(0)),
          ProcessInstruction.Release(Vector(0)),
          Op("x", refs = Vector(Helper(0))),
          ProcessInstruction.Measure(Helper(0)),
          ProcessInstruction.Release(Vector(0))
        )
      )
    )
  }

  test("live-interval process can reuse one physical qubit for non-overlapping logical wires") {
    val circuit =
      Circuit(
        List(
          H(0),
          Measure(0),
          X(1),
          Measure(1)
        ),
        qubits = 2,
        name = "sequential-wires"
      )

    val process = DeviceFitBenchmark.processFromCircuit(circuit)
    val topology = DeviceTopology.fromEdges(Nil, List(0))
    val attempt = HaloCircuitMerger.mergeProcesses(Vector(process), topology)
    val plan = attempt.toOption.get
    val usedPhysicals = plan.deviceCircuit.remainingGates.flatMap {
      case H(q) => List(q)
      case X(q) => List(q)
      case Measure(q) => List(q)
      case Reset(q) => List(q)
      case _ => Nil
    }.toSet

    IO.pure(
      expect(attempt.isRight) and
      expect(usedPhysicals == Set(0)) and
      expect(plan.partitions.head.measurementBitIndices == Vector(0, 1)) and
      expect(plan.partitions.head.measuredRefs == Vector(Helper(0), Helper(1))) and
      expect(plan.helperAssignments.map(_.physicalQubit).distinct == Vector(0))
    )
  }

  test("QASM parser preserves rzz as a supported named two-qubit gate") {
    val qasm =
      """OPENQASM 3.0;
        |include "stdgates.inc";
        |qubit[2] q;
        |rzz(pi/2) q[0], q[1];
        |""".stripMargin

    val report =
      Qasm3Parser.parseWithReport(
        qasm = qasm,
        config = Qasm3Parser.ParseConfig.lenientSkipUnsupported
      )

    IO.pure(
      expect(report.warnings.isEmpty) and
      expect(report.circuit.remainingGates == List(NamedGate("rzz", Vector("pi/2"), Vector(0, 1))))
    )
  }

  test("peakLiveQubits accounts for concurrent merged process lifetimes") {
    val circuit =
      Circuit(
        List(
          H(0),
          CX(0, 1),
          Measure(0),
          Measure(1)
        ),
        qubits = 2,
        name = "two-live"
      )

    val process = DeviceFitBenchmark.processFromCircuit(circuit)

    IO.pure(
      expect(CircuitProcessConverter.peakLiveQubits(Vector(process)) == 2) and
      expect(CircuitProcessConverter.peakLiveQubits(Vector(process, process)) == 4)
    )
  }

  test("run uses all-to-all topology fallback for topology-less IonQ calibrations") {
    implicit val logger = NoOpLogger.impl[IO]

    val client = new ProviderClient[IO] {
      def provider: String = "test"
      def fetchAvailableDevices: IO[List[Device]] =
        IO.pure(List(Device("Braket", "ionq-test", qubits = 4, t1 = 0.0f, t2 = 0.0f, gateSet = Nil)))
      def fetchDeviceCalibration(deviceId: String): IO[DeviceCalibration] =
        IO.pure(
          IonQCalibration(
            t1Seconds = 1.0,
            t2Seconds = 1.0,
            avg1qFidelityPct = 99.0,
            avg2qFidelityPct = 98.0,
            avgReadoutFidelity = 97.0,
            oneQGateDurationSec = 1e-6,
            twoQGateDurationSec = 2e-6,
            readoutDurationSec = 1e-6,
            topology = None
          )
        )
      def submitTask(device: Device, task: QuantumTask, compiled: Circuit): IO[ProviderTaskSubmission] = ???
      def getTask(taskId: String): IO[ProviderTaskStatus] = ???
      def fetchJobTiming(taskId: String, status: ProviderTaskStatus): IO[ProviderJobTiming] = ???
      def fetchTaskResult(taskId: String, status: ProviderTaskStatus): IO[QuantumJobResult] = ???
      def completedStatuses: Set[String] = Set.empty
    }

    val qasm =
      """OPENQASM 3.0;
        |include "stdgates.inc";
        |qubit[1] q;
        |h q[0];
        |q[0] = measure q[0];
        |""".stripMargin

    for {
      dir <- IO.blocking(java.nio.file.Files.createTempDirectory("device-fit-qasm"))
      _ <- IO.blocking(java.nio.file.Files.writeString(dir.resolve("a.qasm"), qasm, StandardCharsets.UTF_8))
      _ <- IO.blocking(java.nio.file.Files.writeString(dir.resolve("b.qasm"), qasm, StandardCharsets.UTF_8))
      report <- DeviceFitBenchmark.run(
        clients = List(client),
        settings = DeviceFitBenchmark.Settings(
          qasmFolder = fs2.io.file.Path.fromNioPath(dir),
          loaderParallelism = 1,
          deviceFetchParallelism = 1
        )
      )
    } yield expect(report.targets.size == 1) and
      expect(report.rows.headOption.exists(_.accommodatingDevices == 1)) and
      expect(report.rows.headOption.flatMap(_.minEstimatedFidelity).exists(_.isFinite))
  }

  test("writeCsv writes benchmark rows to a file") {
    val report =
      DeviceFitBenchmark.Report(
        loadedCircuits = 0,
        parseWarnings = Vector.empty,
        partitions = Vector.empty,
        targets = Vector.empty,
        rows = Vector(
          DeviceFitBenchmark.IterationResult(
            circuitsMerged = 2,
            mappedQubits = 5,
            accommodatingDevices = 3,
            minEstimatedFidelity = Some(0.12),
            minEstimatedFidelityDevice = Some("IBM/device,a"),
            maxEstimatedFidelity = Some(0.98),
            maxEstimatedFidelityDevice = Some("Braket/device-b")
          )
        )
      )

    for {
      tmp <- IO.blocking(java.nio.file.Files.createTempFile("device-fit-benchmark", ".csv"))
      path = fs2.io.file.Path.fromNioPath(tmp)
      _ <- DeviceFitBenchmark.writeCsv(report, path)
      content <- IO.blocking(new String(java.nio.file.Files.readAllBytes(tmp), StandardCharsets.UTF_8))
      _ <- IO.blocking(java.nio.file.Files.deleteIfExists(tmp)).attempt
    } yield expect(content.startsWith(DeviceFitBenchmark.csvHeader)) and
      expect(content.contains("\"IBM/device,a\"")) and
      expect(content.contains("2,5,3"))
  }
}
