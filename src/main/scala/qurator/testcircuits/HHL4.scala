package qurator.testcircuits

object HHL4{
    val qasm = 
        """
        OPENQASM 3.0;
        include "stdgates.inc";
        bit[4] meas;
        qubit[4] q12;
        ry(0) q12[0];
        h q12[1];
        h q12[2];
        p(2.0) q12[1];
        p(4.0) q12[2];
        U(-0.6666666666666666, -pi/2, pi/2) q12[0];
        p(pi/2) q12[0];
        cx q12[1], q12[0];
        ry(-0.6666666666666666) q12[0];
        cx q12[1], q12[0];
        ry(0.6666666666666666) q12[0];
        p(3*pi/2) q12[0];
        p(pi/2) q12[0];
        cx q12[2], q12[0];
        ry(-1.3333333333333333) q12[0];
        cx q12[2], q12[0];
        ry(1.3333333333333333) q12[0];
        p(3*pi/2) q12[0];
        h q12[2];
        rz(-pi/4) q12[2];
        cx q12[1], q12[2];
        rz(pi/4) q12[2];
        cx q12[1], q12[2];
        rz(-pi/4) q12[1];
        h q12[1];
        cx q12[2], q12[3];
        ry(-0.6935172303253598) q12[3];
        cx q12[1], q12[3];
        ry(-0.8772790964695367) q12[3];
        cx q12[2], q12[3];
        ry(0.35368032087123796) q12[3];
        cx q12[1], q12[3];
        ry(1.2171160059236585) q12[3];
        barrier q12[0], q12[1], q12[2], q12[3];
        meas[0] = measure q12[0];
        meas[1] = measure q12[1];
        meas[2] = measure q12[2];
        meas[3] = measure q12[3];
        """
}