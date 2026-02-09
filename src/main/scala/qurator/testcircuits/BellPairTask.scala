package qurator.testcircuits

import qurator.domain.circuit._

object BellPairTask{

    val qubitcount = 2 

    val qasm = "OPENQASM 3.0; include \"stdgates.inc\"; bit[2] c; qubit[2] q; rz(pi/2) q[1]; rx(pi/2) q[1]; rz(pi/2) q[1]; cz q[0], q[1]; rz(pi/2) q[1]; rx(pi/2) q[1]; rz(pi/2) q[1]; rz(pi/2) q[0]; rx(pi/2) q[0]; rz(pi/2) q[0]; c[0] = measure q[0]; c[1] = measure q[1];"
    
}