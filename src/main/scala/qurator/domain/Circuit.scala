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
        def toQasm : String = ???
    }
}