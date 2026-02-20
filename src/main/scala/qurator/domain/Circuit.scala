package qurator.domain

import spire.math._
import spire.implicits._ 

package object circuit{
    trait Gate 
    case class X(q: Int) extends Gate 
    case class H(ctrl: Int) extends Gate 
    case class CX(ctrl: Int, target: Int) extends Gate
    case class CCX(ctrl1: Int, ctrl2: Int, target: Int) extends Gate 
    case class CZ(ctrl: Int, target: Int) extends Gate
    case class U(start: Int, end: Int, power:Int) extends Gate
    case class CU(ctrl: Int, start: Int, end: Int, power:Int) extends Gate
    case class Swap(q1: Int, q2: Int) extends Gate
    case class CRotate(ctrl: Int, thetaDenom: Int, q: Int) extends Gate
    case class Rotate(thetaDenom: Int, q: Int) extends Gate
    case class RZ(thetaDenom: Int, q: Int) extends Gate
    case class Measure(q: Int) extends Gate
    
    final case class Circuit(remainingGates: List[Gate], qubits: Int, name: String = "") 
    final case class QVec(prop: Complex[Double], v: Vector[Boolean], name: String ="")


    implicit class Circuit2Qasm(c: Circuit){
        def toQasm : String = ???
    }
}