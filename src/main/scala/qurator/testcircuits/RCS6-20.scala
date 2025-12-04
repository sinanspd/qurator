package qurator.testcircuits

object RCS6_20{
    val qasm=
        """
        OPENQASM 3.0;
        include "stdgates.inc";
        gate ccz _gate_q_0, _gate_q_1, _gate_q_2 {
        h _gate_q_2;
        ccx _gate_q_0, _gate_q_1, _gate_q_2;
        h _gate_q_2;
        }
        gate sxdg _gate_q_0 {
        s _gate_q_0;
        h _gate_q_0;
        s _gate_q_0;
        }
        gate xx_plus_yy(p0, p1) _gate_q_0, _gate_q_1 {
        rz(p1) _gate_q_0;
        sdg _gate_q_1;
        sx _gate_q_1;
        s _gate_q_1;
        s _gate_q_0;
        cx _gate_q_1, _gate_q_0;
        ry((-0.5)*p0) _gate_q_1;
        ry((-0.5)*p0) _gate_q_0;
        cx _gate_q_1, _gate_q_0;
        sdg _gate_q_0;
        sdg _gate_q_1;
        sxdg _gate_q_1;
        s _gate_q_1;
        rz(-p1) _gate_q_0;
        }
        gate r(p0, p1) _gate_q_0 {
        U(p0, -pi/2 + p1, pi/2 - p1) _gate_q_0;
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
        gate xx_minus_yy(p0, p1) _gate_q_0, _gate_q_1 {
        rz(-p1) _gate_q_1;
        sdg _gate_q_0;
        sx _gate_q_0;
        s _gate_q_0;
        s _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        ry(0.5*p0) _gate_q_0;
        ry((-0.5)*p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        sdg _gate_q_1;
        sdg _gate_q_0;
        sxdg _gate_q_0;
        s _gate_q_0;
        rz(p1) _gate_q_1;
        }
        gate rxx(p0) _gate_q_0, _gate_q_1 {
        h _gate_q_0;
        h _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        rz(p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        h _gate_q_1;
        h _gate_q_0;
        }
        gate cs _gate_q_0, _gate_q_1 {
        t _gate_q_0;
        cx _gate_q_0, _gate_q_1;
        tdg _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        t _gate_q_1;
        }
        gate csx _gate_q_0, _gate_q_1 {
        h _gate_q_1;
        cs _gate_q_0, _gate_q_1;
        h _gate_q_1;
        }
        bit[6] c;
        qubit[6] q;
        cswap q[5], q[1], q[4];
        crx(6.143053073624298) q[3], q[2];
        y q[0];
        u2(6.230609058157182, 5.179635287172941) q[1];
        ccz q[2], q[0], q[3];
        swap q[5], q[4];
        U(3.206846159714529, 4.929275295705789, 3.3893502257628856) q[4];
        ccz q[2], q[3], q[0];
        xx_plus_yy(4.140918461504783, 4.384592360914398) q[5], q[1];
        ccz q[5], q[1], q[4];
        cswap q[3], q[2], q[0];
        cswap q[2], q[1], q[0];
        x q[3];
        crx(1.5453214598356562) q[5], q[4];
        ccx q[0], q[2], q[3];
        r(1.150457269100433, 1.0864688482825107) q[1];
        cswap q[2], q[1], q[3];
        rccx q[0], q[4], q[5];
        rz(2.2242378161085816) q[1];
        xx_minus_yy(5.45366675350985, 2.775700618048709) q[3], q[2];
        tdg q[0];
        r(2.300759082883267, 1.5968210381685044) q[4];
        ry(5.817160384052016) q[0];
        cswap q[1], q[2], q[5];
        cy q[3], q[4];
        ccx q[5], q[3], q[4];
        ccx q[0], q[1], q[2];
        ccz q[4], q[5], q[3];
        t q[1];
        rxx(3.5768014054555177) q[2], q[0];
        ccx q[3], q[1], q[2];
        rccx q[4], q[5], q[0];
        cp(2.6859708750352005) q[1], q[4];
        s q[0];
        cswap q[3], q[5], q[2];
        ccz q[2], q[5], q[4];
        xx_minus_yy(2.5271473791678405, 2.385853231101176) q[0], q[1];
        y q[3];
        rccx q[5], q[0], q[1];
        cswap q[3], q[0], q[1];
        ccx q[2], q[4], q[5];
        csx q[1], q[4];
        crz(3.563448951381172) q[5], q[0];
        ccx q[0], q[4], q[2];
        cswap q[5], q[3], q[1];
        tdg q[2];
        ccx q[4], q[3], q[1];
        s q[0];
        ccx q[3], q[4], q[1];
        xx_plus_yy(1.0194276954440793, 0.36278479289124815) q[2], q[0];
        c[0] = measure q[0];
        c[1] = measure q[1];
        c[2] = measure q[2];
        c[3] = measure q[3];
        c[4] = measure q[4];
        c[5] = measure q[5];
        """
}