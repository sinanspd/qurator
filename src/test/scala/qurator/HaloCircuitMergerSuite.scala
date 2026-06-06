package qurator

import cats.effect.IO
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.testbed.HaqaMapper.DeviceTopology
import qurator.util.HaloCircuitMerger
import qurator.util.HaloCircuitMerger.CountBitOrder
import qurator.util.HaloCircuitMerger.ProcessInstruction
import qurator.util.HaloCircuitMerger.ProcessInstruction.Op
import qurator.util.HaloCircuitMerger.VirtualQubitRef._
import weaver.SimpleIOSuite

object HaloCircuitMergerSuite extends SimpleIOSuite {

  test("mergeProcesses reuses helpers with reset-before-reuse under round-robin scheduling") {
    val topology = DeviceTopology.fromEdges(List(0 -> 1, 1 -> 2))

    val p0 = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 1,
      instructions = Vector(
        Op("cx", refs = Vector(Data(0), Helper(0))),
        ProcessInstruction.Release(Vector(0)),
        ProcessInstruction.Measure(Data(0))
      ),
      name = "p0"
    )

    val p1 = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 1,
      instructions = Vector(
        Op("cx", refs = Vector(Data(0), Helper(0))),
        ProcessInstruction.Release(Vector(0)),
        ProcessInstruction.Measure(Data(0))
      ),
      name = "p1"
    )

    val attempt = HaloCircuitMerger.mergeProcesses(Vector(p0, p1), topology)
    val plan = attempt.toOption.get
    val partitions = plan.partitions.sortBy(_.processIndex)
    val split = plan.splitCounts(Map("01" -> 2L, "10" -> 3L), CountBitOrder.LeftToRight)
    val gateShapeMatches = plan.deviceCircuit.remainingGates match {
      case List(CX(_, _), Reset(_), CX(_, _), Measure(_), Reset(_), Measure(_)) => true
      case _ => false
    }
    val dataPhysicals = partitions.flatMap(_.dataPhysicalQubits)
    val helperPhysicals = plan.helperAssignments.map(_.physicalQubit)

    IO.pure(
      expect(attempt.isRight) and
      expect(gateShapeMatches) and
      expect(plan.helperAssignments.size == 2) and
      expect(helperPhysicals.distinct.size == 1) and
      expect(helperPhysicals.forall(physical => !dataPhysicals.contains(physical))) and
      expect(partitions(0).measurementBitIndices == Vector(0)) and
      expect(partitions(1).measurementBitIndices == Vector(1)) and
      expect(split == Right(Vector(Map("0" -> 2L, "1" -> 3L), Map("1" -> 2L, "0" -> 3L))))
    )
  }

  test("mergeProcesses picks the nearest available helper qubit for a process") {
    val topology = DeviceTopology.fromEdges(List(0 -> 1, 1 -> 2, 2 -> 3))

    val helperHeavy = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 1,
      instructions = Vector(
        Op("cx", refs = Vector(Data(0), Helper(0))),
        ProcessInstruction.Release(Vector(0)),
        ProcessInstruction.Measure(Data(0))
      ),
      name = "helper-heavy"
    )

    val passive = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 0,
      instructions = Vector(
        Op("x", refs = Vector(Data(0))),
        ProcessInstruction.Measure(Data(0))
      ),
      name = "passive"
    )

    val attempt = HaloCircuitMerger.mergeProcesses(Vector(helperHeavy, passive), topology)
    val plan = attempt.toOption.get
    val helperUse = plan.helperAssignments.head
    val partitions = plan.partitions.sortBy(_.processIndex)
    val helperHeavyPhysical = partitions.head.dataPhysicalQubits.head
    val occupied = partitions.flatMap(_.dataPhysicalQubits).toSet
    val freePhysicals = topology.qubits.filterNot(occupied.contains)
    val nearestDistance = freePhysicals.iterator.map(physical => math.abs(physical - helperHeavyPhysical)).min
    val firstGateUsesAssignedHelper =
      plan.deviceCircuit.remainingGates.headOption.exists {
        case CX(control, target) =>
          Set(control, target) == Set(helperHeavyPhysical, helperUse.physicalQubit)
        case _ =>
          false
      }

    IO.pure(
      expect(attempt.isRight) and
      expect(helperUse.processIndex == 0) and
      expect(math.abs(helperUse.physicalQubit - helperHeavyPhysical) == nearestDistance) and
      expect(firstGateUsesAssignedHelper)
    )
  }

  test("mergeProcesses can derive topology from calibration data") {
    val calibration = IQMCalibration(
      t1 = 0.0,
      t2 = 0.0,
      q1fidelity = 0.0,
      q2fidelity = 0.0,
      readoutFidelity = 0.0,
      topology = Some(
        CalibrationTopology(
          qubits = List(0, 1, 2),
          edges = List(0 -> 1, 1 -> 2)
        )
      )
    )

    val process = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 1,
      instructions = Vector(
        Op("cx", refs = Vector(Data(0), Helper(0))),
        ProcessInstruction.Release(Vector(0)),
        ProcessInstruction.Measure(Data(0))
      ),
      name = "calibrated"
    )

    val attempt = HaloCircuitMerger.mergeProcesses(Vector(process), calibration)

    IO.pure(
      expect(attempt.isRight) and
      expect(attempt.toOption.exists(_.topology.qubits == Vector(0, 1, 2))) and
      expect(attempt.toOption.exists(_.partitions.head.measurementBitIndices == Vector(0)))
    )
  }

  test("mergeProcesses seeds the first helper-using process near the topology center when local search is disabled") {
    val topology = DeviceTopology.fromEdges(List(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4))

    val helperHeavy = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 1,
      instructions = Vector(
        Op("cx", refs = Vector(Data(0), Helper(0))),
        ProcessInstruction.Release(Vector(0)),
        ProcessInstruction.Measure(Data(0))
      ),
      name = "helper-heavy"
    )

    val passive = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 0,
      instructions = Vector(Op("x", refs = Vector(Data(0)))),
      name = "passive"
    )

    val config = HaloCircuitMerger.Config(
      maxLocalSearchPasses = 0,
      interProcessIsolationWeight = 0.0
    )

    val attempt = HaloCircuitMerger.mergeProcesses(Vector(helperHeavy, passive), topology, config)
    val plan = attempt.toOption.get

    IO.pure(
      expect(attempt.isRight) and
      expect(plan.partitions.sortBy(_.processIndex).head.dataPhysicalQubits == Vector(2))
    )
  }

  test("mergeProcesses prefers high-readout physical qubits for measured data when calibration is available") {
    val calibration = IQMCalibration(
      t1 = 0.0,
      t2 = 0.0,
      q1fidelity = 0.0,
      q2fidelity = 0.0,
      readoutFidelity = 0.0,
      topology = Some(
        CalibrationTopology(
          qubits = List(0, 1, 2),
          edges = List(0 -> 1, 1 -> 2)
        )
      ),
      qubitMetrics = Map(
        0 -> QubitCalibrationMetrics(readoutFidelity = Some(0.99)),
        1 -> QubitCalibrationMetrics(readoutFidelity = Some(0.50)),
        2 -> QubitCalibrationMetrics(readoutFidelity = Some(0.50))
      )
    )

    val process = HaloCircuitMerger.ProcessCircuit(
      dataQubits = 1,
      helperQubits = 0,
      instructions = Vector(ProcessInstruction.Measure(Data(0))),
      name = "measured"
    )

    val config = HaloCircuitMerger.Config(
      firstSeedCentralityWeight = 0.0,
      firstSeedHelperSlackWeight = 0.0,
      interProcessIsolationWeight = 0.0,
      measurementCrosstalkWeight = 0.0,
      measurementReadoutWeight = 1.0,
      maxLocalSearchPasses = 0
    )

    val attempt = HaloCircuitMerger.mergeProcesses(Vector(process), calibration, config)
    val plan = attempt.toOption.get

    IO.pure(
      expect(attempt.isRight) and
      expect(plan.partitions.head.dataPhysicalQubits == Vector(0))
    )
  }
}
