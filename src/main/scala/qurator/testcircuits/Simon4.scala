package qurator.testcircuits

object Simon4{
    val qasm = 
        """
        OPENQASM 3.0;
        include "stdgates.inc";
        bit[4] c;
        qubit[8] q;
        h q[0];
        h q[1];
        h q[2];
        h q[3];
        cx q[0], q[4];
        cx q[1], q[5];
        cx q[2], q[6];
        cx q[3], q[7];
        h q[0];
        h q[1];
        h q[2];
        h q[3];
        c[0] = measure q[0];
        c[1] = measure q[1];
        c[2] = measure q[2];
        c[3] = measure q[3];
        """
}