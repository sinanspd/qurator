package qurator

import cats.effect.IO
import qurator.domain.circuit._
import qurator.testbed.DeviceFitBenchmark
import qurator.util.HaloCircuitMerger.ProcessInstruction
import qurator.util.HaloCircuitMerger.ProcessInstruction.Op
import qurator.util.HaloCircuitMerger.VirtualQubitRef.Data
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

  test("processFromCircuit preserves gate operands as HALO data references") {
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
      expect(process.dataQubits == 2) and
      expect(process.helperQubits == 0) and
      expect(
        process.instructions == Vector(
          Op("h", refs = Vector(Data(0))),
          Op("cx", refs = Vector(Data(0), Data(1))),
          Op("crz", params = Vector("pi/4"), refs = Vector(Data(1), Data(0))),
          ProcessInstruction.Measure(Data(1)),
          ProcessInstruction.Reset(Data(0))
        )
      )
    )
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
