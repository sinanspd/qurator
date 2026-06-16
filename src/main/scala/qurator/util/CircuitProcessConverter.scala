package qurator.util

import qurator.domain.circuit._
import qurator.util.HaloCircuitMerger.ProcessCircuit
import qurator.util.HaloCircuitMerger.ProcessInstruction
import qurator.util.HaloCircuitMerger.ProcessInstruction.Op
import qurator.util.HaloCircuitMerger.VirtualQubitRef
import qurator.util.HaloCircuitMerger.VirtualQubitRef.{Data, Helper}

import scala.collection.mutable

object CircuitProcessConverter {

  def staticProcessFromCircuit(circuit: Circuit): ProcessCircuit =
    ProcessCircuit(
      dataQubits = logicalQubits(circuit),
      helperQubits = 0,
      instructions = circuit.remainingGates.flatMap(instructionsFromGate(_, Data(_))).toVector,
      name = circuit.name
    )

  def liveIntervalProcessFromCircuit(circuit: Circuit): ProcessCircuit = {
    val qubits = logicalQubits(circuit)
    val gates = circuit.remainingGates.toVector
    val remainingUses = Array.fill(qubits)(0)

    gates.foreach { gate =>
      gateQubits(gate).distinct.foreach { q =>
        if (q >= 0 && q < qubits) remainingUses(q) += 1
      }
    }

    val instructions = mutable.ArrayBuffer.empty[ProcessInstruction]

    gates.foreach { gate =>
      instructions ++= instructionsFromGate(gate, Helper(_))

      val touched = gateQubits(gate).distinct.filter(q => q >= 0 && q < qubits)
      touched.foreach(q => remainingUses(q) -= 1)

      val releasable =
        touched.filter { q =>
          remainingUses(q) == 0 || isResetOf(gate, q)
        }.sorted

      if (releasable.nonEmpty) {
        instructions += ProcessInstruction.Release(releasable.toVector)
      }
    }

    ProcessCircuit(
      dataQubits = 0,
      helperQubits = qubits,
      instructions = instructions.toVector,
      name = circuit.name
    )
  }

  def maxInstructionWidth(process: ProcessCircuit): Int =
    process.instructions.iterator.map(_.refs.distinct.size).maxOption.getOrElse(0)

  def gateQubits(gate: Gate): Vector[Int] =
    gate match {
      case X(q) => Vector(q)
      case Y(q) => Vector(q)
      case Z(q) => Vector(q)
      case H(q) => Vector(q)
      case S(q) => Vector(q)
      case SDG(q) => Vector(q)
      case T(q) => Vector(q)
      case TDG(q) => Vector(q)
      case SX(q) => Vector(q)
      case SXDG(q) => Vector(q)
      case Id(q) => Vector(q)
      case Phase(_, q) => Vector(q)
      case RX(_, q) => Vector(q)
      case RY(_, q) => Vector(q)
      case RZ(_, q) => Vector(q)
      case U(_, _, _, q) => Vector(q)
      case U2(_, _, q) => Vector(q)
      case U3(_, _, _, q) => Vector(q)
      case CX(c, t) => Vector(c, t)
      case CY(c, t) => Vector(c, t)
      case CZ(c, t) => Vector(c, t)
      case CH(c, t) => Vector(c, t)
      case Swap(a, b) => Vector(a, b)
      case CP(c, _, t) => Vector(c, t)
      case CRX(c, _, t) => Vector(c, t)
      case CRY(c, _, t) => Vector(c, t)
      case CRZ(c, _, t) => Vector(c, t)
      case CU(c, _, _, _, t) => Vector(c, t)
      case CCX(a, b, t) => Vector(a, b, t)
      case Measure(q) => Vector(q)
      case Reset(q) => Vector(q)
      case GPhase(_) => Vector.empty
      case NamedGate(_, _, qubits) => qubits
    }

  private def instructionsFromGate(
      gate: Gate,
      ref: Int => VirtualQubitRef
  ): List[ProcessInstruction] =
    gate match {
      case X(q)        => List(Op("x", refs = Vector(ref(q))))
      case Y(q)        => List(Op("y", refs = Vector(ref(q))))
      case Z(q)        => List(Op("z", refs = Vector(ref(q))))
      case H(q)        => List(Op("h", refs = Vector(ref(q))))
      case S(q)        => List(Op("s", refs = Vector(ref(q))))
      case SDG(q)      => List(Op("sdg", refs = Vector(ref(q))))
      case T(q)        => List(Op("t", refs = Vector(ref(q))))
      case TDG(q)      => List(Op("tdg", refs = Vector(ref(q))))
      case SX(q)       => List(Op("sx", refs = Vector(ref(q))))
      case SXDG(q)     => List(Op("sxdg", refs = Vector(ref(q))))
      case Id(q)       => List(Op("id", refs = Vector(ref(q))))
      case Phase(t, q) => List(Op("p", params = Vector(t), refs = Vector(ref(q))))
      case RX(t, q)    => List(Op("rx", params = Vector(t), refs = Vector(ref(q))))
      case RY(t, q)    => List(Op("ry", params = Vector(t), refs = Vector(ref(q))))
      case RZ(t, q)    => List(Op("rz", params = Vector(t), refs = Vector(ref(q))))
      case U(t, p, l, q) =>
        List(Op("u", params = Vector(t, p, l), refs = Vector(ref(q))))
      case U2(p, l, q) =>
        List(Op("u2", params = Vector(p, l), refs = Vector(ref(q))))
      case U3(t, p, l, q) =>
        List(Op("u3", params = Vector(t, p, l), refs = Vector(ref(q))))
      case CX(c, t)       => List(Op("cx", refs = Vector(ref(c), ref(t))))
      case CY(c, t)       => List(Op("cy", refs = Vector(ref(c), ref(t))))
      case CZ(c, t)       => List(Op("cz", refs = Vector(ref(c), ref(t))))
      case CH(c, t)       => List(Op("ch", refs = Vector(ref(c), ref(t))))
      case Swap(a, b)     => List(Op("swap", refs = Vector(ref(a), ref(b))))
      case CP(c, t, q)    => List(Op("cp", params = Vector(t), refs = Vector(ref(c), ref(q))))
      case CRX(c, t, q)   => List(Op("crx", params = Vector(t), refs = Vector(ref(c), ref(q))))
      case CRY(c, t, q)   => List(Op("cry", params = Vector(t), refs = Vector(ref(c), ref(q))))
      case CRZ(c, t, q)   => List(Op("crz", params = Vector(t), refs = Vector(ref(c), ref(q))))
      case CU(c, t, p, l, q) =>
        List(Op("cu", params = Vector(t, p, l), refs = Vector(ref(c), ref(q))))
      case CCX(a, b, t) =>
        List(Op("ccx", refs = Vector(ref(a), ref(b), ref(t))))
      case Measure(q) =>
        List(ProcessInstruction.Measure(ref(q)))
      case Reset(q) =>
        List(ProcessInstruction.Reset(ref(q)))
      case GPhase(_) =>
        Nil
      case NamedGate(name, params, qubits) =>
        List(Op(name, params = params, refs = qubits.map(ref)))
    }

  private def logicalQubits(circuit: Circuit): Int = {
    val maxUsed = circuit.remainingGates.iterator.flatMap(gateQubits).filter(_ >= 0).maxOption.getOrElse(-1)
    math.max(circuit.qubits, maxUsed + 1)
  }

  private def isResetOf(gate: Gate, q: Int): Boolean =
    gate match {
      case Reset(target) => target == q
      case _ => false
    }
}
