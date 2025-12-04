package qurator.testcircuits

object RCS27_65{
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
        gate sxdg _gate_q_0 {
        s _gate_q_0;
        h _gate_q_0;
        s _gate_q_0;
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
        gate dcx _gate_q_0, _gate_q_1 {
        cx _gate_q_0, _gate_q_1;
        cx _gate_q_1, _gate_q_0;
        }
        gate ccz _gate_q_0, _gate_q_1, _gate_q_2 {
        h _gate_q_2;
        ccx _gate_q_0, _gate_q_1, _gate_q_2;
        h _gate_q_2;
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
        gate ryy(p0) _gate_q_0, _gate_q_1 {
        sxdg _gate_q_0;
        sxdg _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        rz(p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        sx _gate_q_0;
        sx _gate_q_1;
        }
        gate r(p0, p1) _gate_q_0 {
        U(p0, -pi/2 + p1, pi/2 - p1) _gate_q_0;
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
        gate csdg _gate_q_0, _gate_q_1 {
        tdg _gate_q_0;
        cx _gate_q_0, _gate_q_1;
        t _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        tdg _gate_q_1;
        }
        gate cu1(p0) _gate_q_0, _gate_q_1 {
        p(0.5*p0) _gate_q_0;
        cx _gate_q_0, _gate_q_1;
        p((-0.5)*p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        p(0.5*p0) _gate_q_1;
        }
        gate cu3(p0, p1, p2) _gate_q_0, _gate_q_1 {
        p(0.5*p1 + 0.5*p2) _gate_q_0;
        p((-0.5)*p1 + 0.5*p2) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        U((-0.5)*p0, 0, (-0.5)*p1 - 0.5*p2) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        U(0.5*p0, p1, 0) _gate_q_1;
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
        gate rzz(p0) _gate_q_0, _gate_q_1 {
        cx _gate_q_0, _gate_q_1;
        rz(p0) _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        }
        gate ecr _gate_q_0, _gate_q_1 {
        s _gate_q_0;
        sx _gate_q_1;
        cx _gate_q_0, _gate_q_1;
        x _gate_q_0;
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
        bit[16] c;
        qubit[16] q;
        rcccx q[1], q[13], q[6], q[8];
        sxdg q[15];
        rcccx q[0], q[11], q[2], q[10];
        rcccx q[3], q[7], q[14], q[9];
        y q[4];
        csx q[5], q[12];
        u3(3.2984885162804733, 0.47116496114183565, 1.4783657952730458) q[15];
        id q[0];
        u3(0.2384587006122412, 3.8657196050711233, 5.0158032758694775) q[12];
        cu(4.691191183223844, 5.759186459253756, 3.7426479728896056, 1.1669921941841723) q[9], q[6];
        tdg q[11];
        ry(4.748377747257056) q[3];
        u1(5.185365194467283) q[13];
        dcx q[1], q[7];
        ccz q[5], q[14], q[8];
        rccx q[4], q[2], q[10];
        cry(3.884844058823445) q[7], q[1];
        cs q[6], q[13];
        cz q[0], q[8];
        swap q[2], q[14];
        rcccx q[10], q[15], q[4], q[11];
        cu(2.0689333166633364, 0.9279758083015347, 0.8661189199394669, 1.5699344559185133) q[9], q[3];
        sdg q[5];
        ccz q[14], q[3], q[0];
        rcccx q[8], q[12], q[10], q[11];
        c3sx q[7], q[1], q[15], q[4];
        cx q[2], q[9];
        cry(1.3641110396124403) q[6], q[13];
        ccx q[0], q[5], q[9];
        c3sx q[14], q[1], q[2], q[10];
        rzx(1.6176581358217694) q[4], q[3];
        iswap q[6], q[15];
        sx q[12];
        cu(2.592973898456994, 1.4336150667609693, 5.37011400066774, 0.7469990747110384) q[13], q[8];
        u1(3.276623580325348) q[11];
        ryy(4.7356923218661375) q[13], q[11];
        rcccx q[6], q[9], q[3], q[2];
        cy q[12], q[0];
        rcccx q[5], q[10], q[1], q[8];
        ccx q[7], q[4], q[14];
        r(4.034819475002662, 3.3033311810255133) q[15];
        c3sx q[6], q[4], q[11], q[5];
        ccz q[14], q[3], q[1];
        rcccx q[13], q[8], q[9], q[2];
        ryy(5.099610165070079) q[7], q[12];
        swap q[15], q[10];
        x q[7];
        c3sx q[13], q[5], q[0], q[6];
        u3(1.587986576913261, 4.623559982862728, 2.10851381230126) q[8];
        swap q[4], q[12];
        h q[3];
        r(1.2140414331808025, 3.3431034593043014) q[14];
        rccx q[11], q[9], q[15];
        ryy(1.7354380015410749) q[10], q[2];
        ccx q[8], q[1], q[14];
        x q[3];
        cs q[11], q[5];
        c3sx q[9], q[7], q[13], q[10];
        c3sx q[2], q[6], q[0], q[15];
        cx q[4], q[12];
        p(1.8771927870927958) q[15];
        rxx(0.015416514043303619) q[10], q[7];
        c3sx q[13], q[8], q[0], q[6];
        csdg q[14], q[5];
        rxx(1.4184326329517412) q[9], q[11];
        rz(1.7630519242995366) q[2];
        c3sx q[1], q[4], q[3], q[12];
        rcccx q[13], q[11], q[15], q[5];
        rcccx q[3], q[4], q[1], q[0];
        c3sx q[12], q[9], q[10], q[14];
        cswap q[7], q[2], q[8];
        csdg q[13], q[8];
        c3sx q[12], q[14], q[1], q[0];
        cu1(4.874925544500802) q[11], q[5];
        rzx(4.198282720758702) q[15], q[4];
        rccx q[10], q[6], q[9];
        crz(1.4772207553407584) q[3], q[7];
        sx q[9];
        dcx q[7], q[5];
        swap q[0], q[14];
        csdg q[8], q[15];
        c3sx q[4], q[11], q[6], q[10];
        ccz q[1], q[3], q[12];
        h q[13];
        ch q[7], q[9];
        cu3(0.5109257871115348, 2.485056776478082, 4.610572171708628) q[1], q[0];
        cs q[14], q[3];
        rcccx q[2], q[12], q[5], q[4];
        rcccx q[15], q[13], q[11], q[8];
        ryy(5.784162988155098) q[10], q[6];
        rxx(1.1693720041873836) q[7], q[1];
        crz(4.997792273807318) q[6], q[0];
        sxdg q[12];
        xx_plus_yy(0.10740724339573968, 0.39384112924598297) q[4], q[15];
        rcccx q[13], q[2], q[5], q[8];
        rcccx q[9], q[14], q[10], q[3];
        rcccx q[1], q[14], q[12], q[11];
        s q[7];
        rcccx q[9], q[6], q[3], q[8];
        rcccx q[2], q[13], q[0], q[5];
        xx_plus_yy(4.560647149553656, 4.023094859846493) q[4], q[10];
        csdg q[3], q[4];
        xx_plus_yy(2.2962944098089766, 1.436310203528472) q[6], q[2];
        c3sx q[5], q[12], q[1], q[14];
        cu(1.445053417506961, 3.186765695539895, 0.5317718604344732, 6.195303495176297) q[8], q[11];
        y q[0];
        cswap q[7], q[15], q[10];
        cx q[13], q[9];
        cu(1.1286671132152521, 2.8496660937246934, 5.38114391766108, 5.905921538182784) q[2], q[7];
        p(4.892305657631169) q[10];
        cry(2.5126679109762735) q[5], q[8];
        rx(3.0717819090733802) q[4];
        sx q[13];
        sdg q[14];
        rcccx q[1], q[12], q[9], q[0];
        xx_plus_yy(5.656951102743126, 4.843707995868187) q[11], q[6];
        cu(4.030590893780426, 1.6704380527476583, 5.652948057990519, 1.756940298026764) q[3], q[15];
        cu3(4.800731621881165, 0.2916674255367656, 3.178520785249049) q[4], q[9];
        ry(0.8487855010105754) q[13];
        rcccx q[5], q[6], q[10], q[7];
        rzx(3.320539838084059) q[14], q[2];
        tdg q[1];
        rcccx q[11], q[3], q[12], q[15];
        cu(0.23256815955990884, 4.768696356004673, 5.436121484175556, 1.3823964567898561) q[0], q[8];
        ch q[3], q[8];
        cu(5.512858338537706, 2.6805115668829105, 0.42760125948918626, 4.505945649378174) q[9], q[12];
        c3sx q[6], q[13], q[0], q[15];
        c3sx q[1], q[14], q[7], q[4];
        cu1(0.9409327697613656) q[11], q[10];
        ch q[5], q[2];
        c3sx q[0], q[8], q[7], q[2];
        swap q[6], q[3];
        c3sx q[5], q[1], q[10], q[11];
        c3sx q[13], q[14], q[9], q[15];
        dcx q[2], q[12];
        crx(3.2941244479552023) q[14], q[4];
        cu(4.832946340807322, 2.0824262390734054, 6.160371590445944, 3.5219516273780345) q[5], q[3];
        cu3(4.001068527075204, 1.7608704004003908, 2.2491345966975294) q[10], q[13];
        iswap q[1], q[11];
        ccz q[6], q[7], q[0];
        cy q[9], q[15];
        rcccx q[15], q[9], q[6], q[3];
        rxx(1.7580318192205087) q[2], q[4];
        cz q[5], q[11];
        p(5.233117266587925) q[7];
        rcccx q[14], q[8], q[0], q[13];
        crx(1.8698640106328122) q[12], q[10];
        ryy(5.541790106739171) q[1], q[13];
        rcccx q[6], q[7], q[5], q[11];
        rcccx q[10], q[0], q[15], q[2];
        crz(2.1479234119074655) q[14], q[3];
        crz(0.36981790915982016) q[8], q[9];
        tdg q[4];
        rcccx q[7], q[12], q[15], q[2];
        c3sx q[11], q[13], q[14], q[4];
        rccx q[0], q[8], q[6];
        U(2.939894019975459, 1.0544161583927467, 0.659028823609602) q[1];
        rccx q[5], q[3], q[10];
        ry(0.9334087704206758) q[9];
        x q[11];
        c3sx q[6], q[8], q[15], q[13];
        rzz(2.117038072684214) q[7], q[1];
        swap q[5], q[3];
        dcx q[0], q[10];
        rccx q[9], q[12], q[2];
        u1(5.711377920831959) q[14];
        c3sx q[13], q[8], q[6], q[9];
        csx q[4], q[0];
        rcccx q[11], q[1], q[14], q[7];
        rzx(2.923277631158417) q[2], q[15];
        rcccx q[3], q[12], q[5], q[10];
        rx(4.0798231137569845) q[8];
        ryy(5.628691702560248) q[5], q[0];
        ry(4.965990749282302) q[6];
        rccx q[14], q[1], q[13];
        c3sx q[4], q[10], q[11], q[9];
        rcccx q[3], q[7], q[12], q[2];
        c3sx q[4], q[10], q[3], q[14];
        cswap q[12], q[2], q[8];
        rcccx q[13], q[15], q[0], q[11];
        cu3(3.7661371418502108, 4.586423817823485, 1.4147891593643784) q[1], q[9];
        ccx q[6], q[7], q[5];
        crx(0.017448438329233172) q[6], q[9];
        csdg q[4], q[10];
        rcccx q[5], q[15], q[3], q[2];
        cx q[0], q[11];
        cz q[12], q[7];
        u1(3.7780094792994308) q[8];
        ecr q[13], q[14];
        id q[1];
        u1(1.0088585539477029) q[13];
        c3sx q[1], q[5], q[0], q[7];
        tdg q[8];
        h q[14];
        cy q[11], q[9];
        u1(5.337693951131369) q[3];
        rcccx q[15], q[12], q[10], q[2];
        cz q[4], q[6];
        rccx q[1], q[9], q[7];
        swap q[15], q[3];
        crz(4.26806689150691) q[5], q[10];
        c3sx q[11], q[6], q[2], q[4];
        csx q[8], q[13];
        csdg q[12], q[0];
        h q[14];
        cs q[11], q[8];
        xx_minus_yy(4.44581063424082, 2.907826828271828) q[2], q[6];
        rcccx q[10], q[1], q[3], q[7];
        rx(1.0383014358348308) q[4];
        c3sx q[12], q[14], q[9], q[5];
        cswap q[13], q[0], q[15];
        c3sx q[9], q[0], q[13], q[10];
        sx q[1];
        c3sx q[12], q[14], q[8], q[4];
        rcccx q[6], q[11], q[7], q[5];
        xx_plus_yy(0.01584476148661856, 0.6322087919038546) q[15], q[2];
        cu3(0.9396540910677846, 0.762173035750632, 1.6134455825238496) q[9], q[4];
        c3sx q[13], q[11], q[12], q[1];
        rccx q[7], q[10], q[6];
        ecr q[3], q[0];
        cp(0.3911883344333415) q[15], q[8];
        cs q[2], q[5];
        rcccx q[15], q[8], q[1], q[6];
        c3sx q[5], q[7], q[11], q[3];
        cu1(3.038391461877326) q[2], q[13];
        ccz q[14], q[4], q[10];
        c3sx q[12], q[2], q[13], q[6];
        rzx(2.3738482934860854) q[11], q[14];
        rcccx q[1], q[5], q[3], q[0];
        csx q[8], q[4];
        c3sx q[15], q[9], q[7], q[10];
        y q[2];
        y q[3];
        rccx q[0], q[11], q[9];
        cry(0.277609758545295) q[12], q[5];
        U(3.6860438177932573, 1.5358217024027883, 1.5792267834568614) q[14];
        c3sx q[13], q[7], q[4], q[8];
        rcccx q[6], q[10], q[15], q[1];
        p(5.474676608418087) q[4];
        rcccx q[10], q[1], q[3], q[15];
        ccz q[2], q[13], q[0];
        ecr q[14], q[6];
        ch q[7], q[8];
        swap q[5], q[9];
        rcccx q[2], q[4], q[0], q[14];
        dcx q[6], q[10];
        rcccx q[12], q[15], q[5], q[11];
        cu(5.490630274579525, 4.695315778005274, 6.227155770690184, 5.860922080954602) q[3], q[7];
        iswap q[1], q[13];
        cs q[8], q[9];
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
        c[11] = measure q[11];
        c[12] = measure q[12];
        c[13] = measure q[13];
        c[14] = measure q[14];
        c[15] = measure q[15];
        """
}