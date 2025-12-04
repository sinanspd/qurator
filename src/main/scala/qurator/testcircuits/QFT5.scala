package qurator.testcircuits

object QFT5{

    val qasm = 
        """
        OPENQASM 3.0;
        include "stdgates.inc";
        gate QFT _gate_q_0, _gate_q_1, _gate_q_2, _gate_q_3, _gate_q_4 {
        h _gate_q_4;
        cp(pi/2) _gate_q_4, _gate_q_3;
        cp(pi/4) _gate_q_4, _gate_q_2;
        cp(pi/8) _gate_q_4, _gate_q_1;
        cp(pi/16) _gate_q_4, _gate_q_0;
        h _gate_q_3;
        cp(pi/2) _gate_q_3, _gate_q_2;
        cp(pi/4) _gate_q_3, _gate_q_1;
        cp(pi/8) _gate_q_3, _gate_q_0;
        h _gate_q_2;
        cp(pi/2) _gate_q_2, _gate_q_1;
        cp(pi/4) _gate_q_2, _gate_q_0;
        h _gate_q_1;
        cp(pi/2) _gate_q_1, _gate_q_0;
        h _gate_q_0;
        swap _gate_q_0, _gate_q_4;
        swap _gate_q_1, _gate_q_3;
        }
        bit[5] meas;
        qubit[5] q;
        QFT q[0], q[1], q[2], q[3], q[4];
        barrier q[0], q[1], q[2], q[3], q[4];
        meas[0] = measure q[0];
        meas[1] = measure q[1];
        meas[2] = measure q[2];
        meas[3] = measure q[3];
        meas[4] = measure q[4];
        """
}