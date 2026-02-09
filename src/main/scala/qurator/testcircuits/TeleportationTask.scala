package qurator.testcircuits

import qurator.domain.circuit._

object TeleportationTask{

    val qubitcount = 3

    val qasm = """
        OPENQASM 3.0;
        include "stdgates.inc";
        bit[3] c;
        qubit[3] q;
        U(0.9688805258925334, 5.4240092003643845, 0) q[0];
        barrier q[0], q[1], q[2];
        h q[1];
        cx q[1], q[2];
        barrier q[0], q[1], q[2];
        cx q[0], q[1];
        h q[0];
        barrier q[0], q[1], q[2];
        c[0] = measure q[0];
        c[1] = measure q[1];
        if (c[0]) {
            z q[2];
        }
        if (c[1]) {
            x q[2];
        }
        barrier q[0], q[1], q[2];
        U(-0.9688805258925334, 0, -5.4240092003643845) q[2];
        c[2] = measure q[2];
    """

}