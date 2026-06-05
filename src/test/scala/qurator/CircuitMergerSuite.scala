package qurator

import cats.effect.IO
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.testbed.HaqaMapper.DeviceTopology
import qurator.util.CircuitMerger
import qurator.util.CircuitMerger.CountBitOrder
import weaver.SimpleIOSuite

object CircuitMergerSuite extends SimpleIOSuite {

  test("merge interleaves circuit layers and records measurement partitions") {
    val topology =
      DeviceTopology.fromEdges(List(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5))

    val first = Circuit(
      List(
        H(0),
        CX(0, 1),
        Measure(0),
        Measure(1)
      ),
      qubits = 2,
      name = "first"
    )

    val second = Circuit(
      List(
        X(0),
        Measure(0)
      ),
      qubits = 1,
      name = "second"
    )

    val attempt = CircuitMerger.merge(Vector(first, second), topology)

    val result = attempt.toOption.get
    val partitions = result.partitions.sortBy(_.circuitIndex)
    val firstPartition = partitions(0)
    val secondPartition = partitions(1)
    val splitCounts = result.splitCounts(
      mergedCounts = Map("010" -> 2L, "111" -> 3L),
      bitOrder = CountBitOrder.LeftToRight
    )

    IO.pure(
      expect(attempt.isRight) and
      expect(result.logicalCircuit.qubits == 3) and
      expect(result.logicalCircuit.remainingGates == List(H(0), X(2), CX(0, 1), Measure(2), Measure(0), Measure(1))) and
      expect(result.deviceCircuit.qubits == 6) and
      expect(firstPartition.originalToMergedLogical == Vector(0, 1)) and
      expect(secondPartition.originalToMergedLogical == Vector(2)) and
      expect(firstPartition.measurementBitIndices == Vector(1, 2)) and
      expect(secondPartition.measurementBitIndices == Vector(0)) and
      expect(firstPartition.measuredMergedLogicalQubits == Vector(0, 1)) and
      expect(secondPartition.measuredMergedLogicalQubits == Vector(2)) and
      expect(splitCounts == Right(Vector(Map("10" -> 2L, "11" -> 3L), Map("0" -> 2L, "1" -> 3L))))
    )
  }

  test("merge exposes physical placement on non-contiguous device labels") {
    val topology =
      DeviceTopology.fromEdges(List(10 -> 12, 12 -> 15, 15 -> 18, 18 -> 20))

    val first = Circuit(List(CX(0, 1), Measure(1)), qubits = 2, name = "a")
    val second = Circuit(List(H(0), Measure(0)), qubits = 1, name = "b")

    val attempt = CircuitMerger.merge(Vector(first, second), topology)
    val plan = attempt.toOption.get
    val allowed = topology.qubits.toSet
    val allPlaced = plan.partitions.flatMap(_.originalToPhysical).toSet
    val measuredPhysicals = plan.partitions.flatMap(_.measuredPhysicalQubits).toSet
    val deviceGateQubits = plan.deviceCircuit.remainingGates.collect {
      case H(q)       => List(q)
      case CX(a, b)   => List(a, b)
      case Measure(q) => List(q)
    }.flatten.toSet

    IO.pure(
      expect(attempt.isRight) and
      expect(plan.logicalCircuit.qubits == 3) and
      expect(plan.deviceCircuit.qubits == 21) and
      expect(allPlaced.subsetOf(allowed)) and
      expect(measuredPhysicals.subsetOf(allowed)) and
      expect(deviceGateQubits.subsetOf(allowed))
    )
  }

  test("merge can derive the topology directly from calibration data") {
    val calibration = IQMCalibration(
      t1 = 0.0,
      t2 = 0.0,
      q1fidelity = 0.0,
      q2fidelity = 0.0,
      readoutFidelity = 0.0,
      topology = Some(
        CalibrationTopology(
          qubits = List(0, 1, 2, 3),
          edges = List(0 -> 1, 1 -> 2, 2 -> 3)
        )
      )
    )

    val attempt = CircuitMerger.merge(
      circuits = Vector(
        Circuit(List(H(0), Measure(0)), qubits = 1, name = "x"),
        Circuit(List(CX(0, 1), Measure(1)), qubits = 2, name = "y")
      ),
      calibration = calibration,
      config = CircuitMerger.Config()
    )

    IO.pure(
      expect(attempt.isRight) and
      expect(attempt.toOption.exists(_.topology.qubits == Vector(0, 1, 2, 3))) and
      expect(attempt.toOption.exists(_.partitions.map(_.logicalQubitCount) == Vector(1, 2)))
    )
  }
}
