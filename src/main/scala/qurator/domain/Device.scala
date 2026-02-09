package qurator.domain

import qurator.domain.circuit._

object device{

    case class Device(
        platform: String,
        platformId: String, 
        qubits: Int,
        t1: Float,
        t2: Float,
        gateSet: List[Gate]
    )
}