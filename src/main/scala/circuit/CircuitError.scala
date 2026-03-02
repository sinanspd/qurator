package com.sinanspd.qure.circuit

import scala.util.control.NoStackTrace

object circuitError {
    final case class InvalidCircuitError(m: String) extends NoStackTrace
}