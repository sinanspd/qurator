package qurator.domain

import spire.math._
import spire.implicits._ 

package object circuit{
    
    sealed trait Gate

    final case class X(q: Int) extends Gate
    final case class Y(q: Int) extends Gate
    final case class Z(q: Int) extends Gate
    final case class H(q: Int) extends Gate
    final case class S(q: Int) extends Gate
    final case class SDG(q: Int) extends Gate
    final case class T(q: Int) extends Gate
    final case class TDG(q: Int) extends Gate
    final case class SX(q: Int) extends Gate
    final case class SXDG(q: Int) extends Gate
    final case class Id(q: Int) extends Gate

    final case class Phase(theta: String, q: Int) extends Gate      // p / phase / u1
    final case class RX(theta: String, q: Int) extends Gate
    final case class RY(theta: String, q: Int) extends Gate
    final case class RZ(theta: String, q: Int) extends Gate
    final case class U(theta: String, phi: String, lambda: String, q: Int) extends Gate
    final case class U2(phi: String, lambda: String, q: Int) extends Gate
    final case class U3(theta: String, phi: String, lambda: String, q: Int) extends Gate

    final case class CX(ctrl: Int, target: Int) extends Gate
    final case class CY(ctrl: Int, target: Int) extends Gate
    final case class CZ(ctrl: Int, target: Int) extends Gate
    final case class CH(ctrl: Int, target: Int) extends Gate
    final case class Swap(q1: Int, q2: Int) extends Gate

    final case class CP(ctrl: Int, theta: String, target: Int) extends Gate
    final case class CRX(ctrl: Int, theta: String, target: Int) extends Gate
    final case class CRY(ctrl: Int, theta: String, target: Int) extends Gate
    final case class CRZ(ctrl: Int, theta: String, target: Int) extends Gate
    final case class CU(ctrl: Int, theta: String, phi: String, lambda: String, target: Int) extends Gate

    final case class CCX(ctrl1: Int, ctrl2: Int, target: Int) extends Gate

    final case class Measure(q: Int) extends Gate
    final case class Reset(q: Int) extends Gate
    final case class GPhase(theta: String) extends Gate

    // fallback for custom or unsupported-but-flat gate calls
    final case class NamedGate(name: String, params: Vector[String], qubits: Vector[Int]) extends Gate
        
    final case class Circuit(remainingGates: List[Gate], qubits: Int, name: String = "") 
    final case class QVec(prop: Complex[Double], v: Vector[Boolean], name: String ="")

    implicit class Circuit2Qasm(c: Circuit){
        def toQasm : String = {
            var numBits = 0

            c.remainingGates.map {
                case X(q) => s"x q[$q]"
                case Y(q) => s"y q[$q]"
                case Z(q) => s"z q[$q]"
                case H(q) => s"h q[$q]"
                case S(q) => s"s q[$q]"
                case SDG(q) => s"sdg q[$q]"
                case T(q) => s"t q[$q]"
                case TDG(q) => s"tdg q[$q]"
                case SX(q) => s"sx q[$q]"
                case SXDG(q) => s"sxdg q[$q]"
                case Id(q) => s"id q[$q]"

                case Phase(theta, q) => s"p($theta) q[$q]"
                case RX(theta, q) => s"rx($theta) q[$q]"
                case RY(theta, q) => s"ry($theta) q[$q]"
                case RZ(theta, q) => s"rz($theta) q[$q]"
                case U(theta, phi, lambda, q) => s"U($theta, $phi, $lambda) q[$q]"
                case U2(phi, lambda, q) => s"u2($phi, $lambda) q[$q]"
                case U3(theta, phi, lambda, q) => s"u3($theta, $phi, $lambda) q[$q]"

                case CX(c, t) => s"cx q[$c], q[$t]"
                case CY(c, t) => s"cy q[$c], q[$t]"
                case CZ(c, t) => s"cz q[$c], q[$t]"
                case CH(c, t) => s"ch q[$c], q[$t]"
                case Swap(c, t) => s"swap q[$c], q[$t]"

                case CP(c, theta, t) => s"cp($theta) q[$c], q[$t]"
                case CRX(c, theta, t) => s"crx($theta) q[$c], q[$t]"
                case CRY(c, theta, t) => s"cry($theta) q[$c], q[$t]"
                case CRZ(c, theta, t) => s"crz($theta) q[$c], q[$t]"
                case CU(c, theta, phi, lambda, t) => s"ctrl @ U($theta, $phi, $lambda) q[$c], q[$t]"

                case CCX(c1, c2, t) => s"ccx q[$c1], q[$c2], q[$t]"

                case Measure(q) => {
                    numBits += 1
                    s"bit c$numBits;\nc$numBits = measure q[$q]"
                }
                case Reset(q) => s"reset q[$q]"
                case GPhase(theta) => s"gphase($theta)"

                case NamedGate(name, params, qubits) => s"$name(${params.mkString(", ")}) ${qubits.map(q => s"q[$q]").mkString(", ")}"
            }.mkString(s"""OPENQASM 3.0;
                |include "stdgates.inc";
                |qubit[${c.qubits}] q;
                |""".stripMargin, ";\n", ";\n")
        }
    }
}
