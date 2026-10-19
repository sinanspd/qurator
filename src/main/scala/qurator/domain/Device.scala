package qurator.domain

import qurator.domain.circuit._

object device{

    // Unknown capabilities are deliberately unsupported. Gate-set membership
    // alone does not establish that measurement/reset/reuse is legal mid-circuit.
    case class QubitReuseCapabilities(
        midCircuitMeasurement: Boolean = false,
        reset: Boolean = false,
        qubitReuse: Boolean = false
    ) {
        def supportsLiveIntervals: Boolean = midCircuitMeasurement && reset && qubitReuse
    }

    case class Device(
        platform: String,
        platformId: String, 
        qubits: Int,
        queueLength : Int = 0,
        t1: Float,
        t2: Float,
        gateSet: List[Gate],
        qubitReuseCapabilities: QubitReuseCapabilities = QubitReuseCapabilities()
    )
}
