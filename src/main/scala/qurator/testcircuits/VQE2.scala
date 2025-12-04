package qurator.testcircuits

object VQE2{
    val qasm = 
        """
        OPENQASM 3.0;
        include "stdgates.inc";
        rz(pi/2) $3;
        sx $3;
        rz(3*pi/2) $3;
        rz(5.40571741730939) $3;
        x $3;
        rz(pi/2) $4;
        sx $4;
        rz(3*pi/2) $4;
        rz(1.3915735063309604) $4;
        x $4;
        cx $4, $3;
        rz(pi/2) $3;
        sx $3;
        rz(3*pi/2) $3;
        rz(1.2706734175078416) $3;
        x $3;
        rz(pi/2) $4;
        sx $4;
        rz(3*pi/2) $4;
        rz(5.601364250008411) $4;
        x $4;
        """
}