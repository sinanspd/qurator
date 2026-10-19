package qurator

import cats.effect.IO
import qurator.domain.circuit._
import qurator.domain.device.QubitReuseCapabilities
import qurator.util.CircuitProcessConverter
import qurator.util.CircuitProcessConverter.OutputContract
import qurator.util.HaloCircuitMerger
import qurator.util.HaloCircuitMerger.ProcessInstruction
import qurator.util.HaloCircuitMerger.VirtualQubitRef.Helper
import qurator.testbed.HaqaMapper.DeviceTopology
import weaver.SimpleIOSuite

object CircuitProcessConverterSuite extends SimpleIOSuite {
  private val supported = QubitReuseCapabilities(true, true, true)
  private val measured = Circuit(List(H(0), Measure(0), X(1), Measure(1)), 2)

  test("unknown or partially supported backends use static conversion") {
    val capabilities = List(QubitReuseCapabilities(), supported.copy(midCircuitMeasurement = false),
      supported.copy(reset = false), supported.copy(qubitReuse = false))
    val conversions = capabilities.map(CircuitProcessConverter.convertLiveIntervals(measured, _))
    IO.pure(expect(conversions.forall(c => !c.usedLiveIntervals && c.fallbackReason.nonEmpty &&
      c.process == CircuitProcessConverter.staticProcessFromCircuit(measured))))
  }

  test("unmeasured entangled outputs are preserved, including unused declared outputs") {
    val circuits = List(Circuit(List(H(0), CX(0, 1), Measure(1)), 2),
      Circuit(List(H(0), Measure(0)), 2), Circuit(List(H(0), Measure(0), X(0)), 1))
    IO.pure(expect(circuits.forall { circuit =>
      val result = CircuitProcessConverter.convertLiveIntervals(circuit, supported)
      !result.usedLiveIntervals && result.process == CircuitProcessConverter.staticProcessFromCircuit(circuit)
    }))
  }

  test("explicit quantum outputs are kept even after measurement") {
    val result = CircuitProcessConverter.convertLiveIntervals(measured, supported,
      OutputContract(quantumOutputs = Set(0)))
    IO.pure(expect(!result.usedLiveIntervals) and expect(result.process.helperQubits == 0))
  }

  test("explicitly discarded wires may be released without an entanglement test") {
    val circuit = Circuit(List(H(0), CX(0, 1), H(1), Measure(1)), 2)
    val result = CircuitProcessConverter.convertLiveIntervals(circuit, supported,
      OutputContract(discardedQubits = Set(0)))
    IO.pure(expect(result.usedLiveIntervals) and
      expect(result.process.instructions(2) == ProcessInstruction.Release(Vector(0))))
  }

  test("supported measured outputs retain both measurement records while reusing one qubit") {
    val converted = CircuitProcessConverter.convertLiveIntervals(measured, supported)
    val plan = HaloCircuitMerger.mergeProcesses(Vector(converted.process),
      DeviceTopology.fromEdges(Nil, List(0))).toOption.get
    IO.pure(expect(converted.usedLiveIntervals) and
      expect(plan.partitions.head.measuredRefs == Vector(Helper(0), Helper(1))) and
      expect(plan.partitions.head.measurementBitIndices == Vector(0, 1)) and
      expect(plan.helperAssignments.map(_.physicalQubit).toSet == Set(0)))
  }

  test("explicit reset ends a lifetime but an unmeasured final reset remains an output") {
    val circuit = Circuit(List(H(0), Reset(0), X(0), Measure(0)), 1)
    val result = CircuitProcessConverter.convertLiveIntervals(circuit, supported)
    val finalReset = CircuitProcessConverter.convertLiveIntervals(Circuit(List(H(0), Reset(0)), 1), supported)
    IO.pure(expect(result.usedLiveIntervals) and
      expect(result.process.instructions.count(_ == ProcessInstruction.Release(Vector(0))) == 2) and
      expect(!finalReset.usedLiveIntervals))
  }

  test("contradictory or out-of-range output contracts are rejected") {
    val results = List(OutputContract(Set(0), Set(0)), OutputContract(Set(2)), OutputContract(discardedQubits = Set(-1)))
      .map(c => scala.util.Try(CircuitProcessConverter.convertLiveIntervals(measured, supported, c)))
    IO.pure(expect(results.forall(_.isFailure)))
  }
}
