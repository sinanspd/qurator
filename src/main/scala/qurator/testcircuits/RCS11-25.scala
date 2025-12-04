package qurator.testcircuits

object RCS11_25{
  val qasm =
    """
        OPENQASM 3.0;
        include "stdgates.inc";
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
        gate sxdg _gate_q_0 {
        s _gate_q_0;
        h _gate_q_0;
        s _gate_q_0;
        }
        gate ryy(p0) _gate_q_0, _gate_q_1 {
        sxdg _gate_q_0;
        sxdg _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        rz(p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        sx _gate_q_0;
        sx _gate_q_1;
        }
        gate cs _gate_q_0, _gate_q_1 {
        t _gate_q_0;
        cx _gate_q_0, _gate_q_1;
        tdg _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        t _gate_q_1;
        }
        gate csdg _gate_q_0, _gate_q_1 {
        tdg _gate_q_0;
        cx _gate_q_0, _gate_q_1;
        t _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        tdg _gate_q_1;
        }
        gate csx _gate_q_0, _gate_q_1 {
        h _gate_q_1;
        cs _gate_q_0, _gate_q_1;
        h _gate_q_1;
        }
        gate ccz _gate_q_0, _gate_q_1, _gate_q_2 {
        h _gate_q_2;
        ccx _gate_q_0, _gate_q_1, _gate_q_2;
        h _gate_q_2;
        }
        gate cu1(p0) _gate_q_0, _gate_q_1 {
        p(0.5*p0) _gate_q_0;
        cx _gate_q_0, _gate_q_1;
        p((-0.5)*p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        p(0.5*p0) _gate_q_1;
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
        gate ecr _gate_q_0, _gate_q_1 {
        s _gate_q_0;
        sx _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        x _gate_q_0;
        }
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
        gate rzx(p0) _gate_q_0, _gate_q_1 {
        h _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        rz(p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        h _gate_q_1;
        }
        gate iswap _gate_q_0, _gate_q_1 {
        s _gate_q_0;
        s _gate_q_1;
        h _gate_q_0;
        cx _gate_q_0, _gate_q_1;
        cx _gate_q_1, _gate_q_0;
        h _gate_q_1;
        }
        gate cu3(p0, p1, p2) _gate_q_0, _gate_q_1 {
        p(0.5*p1 + 0.5*p2) _gate_q_0;
        p((-0.5)*p1 + 0.5*p2) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        U((-0.5)*p0, 0, (-0.5)*p1 - 0.5*p2) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        U(0.5*p0, p1, 0) _gate_q_1;
        }
        gate rzz(p0) _gate_q_0, _gate_q_1 {
        cx _gate_q_0, _gate_q_1;
        rz(p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
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
        bit[11] c;
        qubit[11] q;
        cswap q[4], q[0], q[5];
        rcccx q[10], q[1], q[8], q[7];
        rccx q[9], q[2], q[3];
        rz(5.742713030754792) q[6];
        ryy(4.136301719977895) q[3], q[6];
        cswap q[0], q[9], q[8];
        cs q[2], q[7];
        csdg q[10], q[5];
        id q[1];
        u2(5.356905704481648, 2.0987455457895257) q[4];
        rccx q[0], q[7], q[2];
        csx q[1], q[4];
        rcccx q[5], q[6], q[8], q[3];
        ry(1.1812615494415624) q[10];
        rz(4.007040819063465) q[9];
        ccx q[9], q[8], q[0];
        rcccx q[4], q[5], q[7], q[1];
        cx q[10], q[3];
        u3(4.242821228481648, 6.081308155700697, 5.744200263251214) q[6];
        ccx q[7], q[4], q[5];
        cx q[2], q[9];
        ccz q[3], q[8], q[1];
        ccz q[10], q[0], q[6];
        cswap q[5], q[3], q[10];
        ccx q[2], q[9], q[6];
        csdg q[4], q[1];
        t q[7];
        cu1(2.711340055730014) q[0], q[8];
        rxx(2.6411430151569912) q[10], q[3];
        ry(5.663230611599401) q[9];
        rccx q[5], q[2], q[7];
        ecr q[8], q[4];
        rccx q[6], q[1], q[0];
        s q[0];
        u2(1.3032126246120326, 2.620369376878096) q[10];
        ccz q[4], q[3], q[5];
        csx q[1], q[7];
        ch q[2], q[9];
        x q[6];
        p(5.493904796753875) q[8];
        cu1(3.115024966839505) q[5], q[7];
        rx(0.3860276799212244) q[2];
        ccz q[0], q[6], q[10];
        cswap q[9], q[8], q[1];
        u1(3.9353933104293506) q[3];
        ccx q[10], q[8], q[1];
        U(0.008994531305820928, 1.1888738590120842, 1.9149119191856419) q[0];
        cswap q[6], q[5], q[2];
        ryy(3.4459788417840382) q[3], q[4];
        z q[7];
        sxdg q[9];
        c3sx q[2], q[7], q[5], q[10];
        rcccx q[3], q[8], q[1], q[6];
        ccx q[4], q[0], q[9];
        rccx q[8], q[2], q[10];
        cswap q[0], q[9], q[5];
        ccx q[6], q[7], q[3];
        rxx(3.915024705714005) q[1], q[4];
        ccx q[1], q[2], q[5];
        cu(2.1996436304893097, 3.0417521896169997, 3.7858513663472486, 1.7047344605306993) q[3], q[4];
        cswap q[7], q[0], q[6];
        t q[8];
        rzx(0.6717170114609554) q[10], q[9];
        rccx q[2], q[7], q[1];
        ccx q[9], q[8], q[10];
        ccz q[6], q[4], q[5];
        crx(6.260050569561306) q[0], q[3];
        iswap q[8], q[3];
        ccx q[10], q[9], q[0];
        rx(5.6643315104458845) q[4];
        ccx q[5], q[2], q[1];
        sx q[6];
        u3(4.814988084183808, 3.556475083724002, 1.5614002714176953) q[7];
        ccz q[3], q[4], q[7];
        rccx q[2], q[10], q[6];
        z q[5];
        rcccx q[9], q[8], q[1], q[0];
        cu3(3.729552171187536, 2.037507840461291, 3.3802356574120127) q[1], q[7];
        rxx(5.342904867577043) q[9], q[4];
        U(3.147297462530787, 2.3562738239729484, 4.0329127718757025) q[5];
        cswap q[6], q[8], q[2];
        rccx q[3], q[10], q[0];
        ccx q[4], q[0], q[8];
        u3(1.076353671475384, 2.754911134076946, 5.098619982177099) q[6];
        ccz q[3], q[2], q[10];
        ccx q[5], q[9], q[1];
        y q[7];
        ccz q[3], q[10], q[2];
        ccx q[6], q[5], q[4];
        ccx q[7], q[0], q[9];
        cu(1.248519452769531, 2.228834093649821, 5.410498649707842, 1.5536948355231808) q[1], q[8];
        z q[5];
        ccx q[2], q[3], q[8];
        rz(4.477084776510579) q[4];
        ccz q[10], q[0], q[7];
        csx q[9], q[1];
        x q[6];
        crx(4.390994227562787) q[2], q[5];
        rzz(4.10860506324775) q[0], q[7];
        rcccx q[10], q[4], q[6], q[1];
        t q[3];
        ryy(3.0989254206340475) q[8], q[9];
        z q[6];
        ry(5.76137441602021) q[10];
        cu1(2.593515374514437) q[9], q[4];
        xx_plus_yy(5.886932879222611, 3.130412761424664) q[7], q[1];
        ch q[2], q[5];
        ccz q[0], q[3], q[8];
        rccx q[0], q[1], q[7];
        id q[9];
        sx q[10];
        cu3(5.466691400390973, 1.8902431464013074, 2.7920235956100066) q[3], q[4];
        cswap q[2], q[6], q[8];
        cu(0.18106017844788602, 0.5604159100590532, 3.17213157838938, 3.897041969741516) q[1], q[2];
        t q[3];
        cswap q[9], q[7], q[10];
        ccz q[8], q[5], q[6];
        rzz(1.0803693596927058) q[0], q[4];
        cswap q[2], q[7], q[3];
        rccx q[0], q[1], q[8];
        x q[9];
        h q[4];
        cz q[5], q[10];
        u2(3.9021719775727095, 3.763419377526147) q[6];
        c[0] = measure q[0];
        c[1] = measure q[1];
        c[2] = measure q[2];
        c[3] = measure q[3];
        c[4] = measure q[4];
        c[5] = measure q[5];
        c[6] = measure q[6];
        c[7] = measure q[7];
        c[8] = measure q[8];
        c[9] = measure q[9];
        c[10] = measure q[10];
    """
}