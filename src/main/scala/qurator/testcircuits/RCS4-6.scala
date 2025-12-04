package qurator.testcircuits

object RCS4_6{
    val qasm = 
        """
        OPENQASM 3.0;
        include "stdgates.inc";
        gate c3sx _gate_q_0, _gate_q_1, _gate_q_2, _gate_q_3 {
        h _gate_q_3;
        cp(pi/8) _gate_q_0, _gate_q_3;
        h _gate_q_3;
        cx _gate_q_0, _gate_q_1;
        h _gate_q_3;
        cp(-pi/8) _gate_q_1, _gate_q_3;
        h _gate_q_3;
        cx _gate_q_0, _gate_q_1;
        h _gate_q_3;
        cp(pi/8) _gate_q_1, _gate_q_3;
        h _gate_q_3;
        cx _gate_q_1, _gate_q_2;
        h _gate_q_3;
        cp(-pi/8) _gate_q_2, _gate_q_3;
        h _gate_q_3;
        cx _gate_q_0, _gate_q_2;
        h _gate_q_3;
        cp(pi/8) _gate_q_2, _gate_q_3;
        h _gate_q_3;
        cx _gate_q_1, _gate_q_2;
        h _gate_q_3;
        cp(-pi/8) _gate_q_2, _gate_q_3;
        h _gate_q_3;
        cx _gate_q_0, _gate_q_2;
        h _gate_q_3;
        cp(pi/8) _gate_q_2, _gate_q_3;
        h _gate_q_3;
        }
        gate rccx _gate_q_0, _gate_q_1, _gate_q_2 {
        h _gate_q_2;
        t _gate_q_2;
        cx _gate_q_1, _gate_q_2;
        tdg _gate_q_2;
        cx _gate_q_0, _gate_q_2;
        t _gate_q_2;
        cx _gate_q_1, _gate_q_2;
        tdg _gate_q_2;
        h _gate_q_2;
        }
        gate rcccx _gate_q_0, _gate_q_1, _gate_q_2, _gate_q_3 {
        h _gate_q_3;
        t _gate_q_3;
        cx _gate_q_2, _gate_q_3;
        tdg _gate_q_3;
        h _gate_q_3;
        cx _gate_q_0, _gate_q_3;
        t _gate_q_3;
        cx _gate_q_1, _gate_q_3;
        tdg _gate_q_3;
        cx _gate_q_0, _gate_q_3;
        t _gate_q_3;
        cx _gate_q_1, _gate_q_3;
        tdg _gate_q_3;
        h _gate_q_3;
        t _gate_q_3;
        cx _gate_q_2, _gate_q_3;
        tdg _gate_q_3;
        h _gate_q_3;
        }
        gate ccz _gate_q_0, _gate_q_1, _gate_q_2 {
        h _gate_q_2;
        ccx _gate_q_0, _gate_q_1, _gate_q_2;
        h _gate_q_2;
        }
        bit[4] c;
        qubit[4] q;
        c3sx q[2], q[1], q[0], q[3];
        u3(3.1234771166480932, 5.528320591062619, 1.6690781307224951) q[2];
        cswap q[1], q[3], q[0];
        rccx q[0], q[2], q[1];
        rcccx q[2], q[1], q[3], q[0];
        ccz q[0], q[1], q[2];
        rccx q[3], q[0], q[2];
        c3sx q[0], q[2], q[3], q[1];
        ccz q[2], q[1], q[0];
        c3sx q[0], q[2], q[3], q[1];
        c3sx q[3], q[0], q[1], q[2];
        c[0] = measure q[0];
        c[1] = measure q[1];
        c[2] = measure q[2];
        c[3] = measure q[3];
        """
}