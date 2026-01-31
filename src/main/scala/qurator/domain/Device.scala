package qurator.domain

import com.sinanspd.qure.circuit.gates.Gate

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