package com.sinanspd.qure

import circuit.gates._
import spire.math._
import spire.implicits._ 

package object circuit{
    final case class Circuit(remainingGates: List[Gate], name: String = "") 
    final case class QVec(prop: Complex[Double], v: Vector[Boolean], name: String ="")
}