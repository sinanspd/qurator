// Benchmark created by MQT Bench on 2025-12-04
// For more info: https://www.cda.cit.tum.de/mqtbench/
// MQT Bench version: 2.1.0
// Qiskit version: 2.1.1
// Output format: qasm3
// Level: nativegates
// Target: ibm_heron
// Used gateset: ['id', 'x', 'sx', 'rz', 'cz', 'reset', 'delay', 'measure']

OPENQASM 3.0;
include "stdgates.inc";
bit[20] c;
qubit[21] q;
x q[0];
rz(pi/2) q[2];
sx q[2];
rz(pi) q[2];
cz q[2], q[0];
sx q[2];
rz(pi/2) q[2];
rz(pi/2) q[4];
sx q[4];
rz(pi) q[4];
cz q[4], q[0];
sx q[4];
rz(pi/2) q[4];
rz(pi/2) q[6];
sx q[6];
rz(pi) q[6];
cz q[6], q[0];
sx q[6];
rz(pi/2) q[6];
rz(pi/2) q[8];
sx q[8];
rz(pi) q[8];
cz q[8], q[0];
sx q[8];
rz(pi/2) q[8];
rz(pi/2) q[10];
sx q[10];
rz(pi) q[10];
cz q[10], q[0];
sx q[10];
rz(pi/2) q[10];
rz(pi/2) q[12];
sx q[12];
rz(pi) q[12];
cz q[12], q[0];
sx q[12];
rz(pi/2) q[12];
rz(pi/2) q[14];
sx q[14];
rz(pi) q[14];
cz q[14], q[0];
sx q[14];
rz(pi/2) q[14];
rz(pi/2) q[16];
sx q[16];
rz(pi) q[16];
cz q[16], q[0];
sx q[16];
rz(pi/2) q[16];
rz(pi/2) q[18];
sx q[18];
rz(pi) q[18];
cz q[18], q[0];
sx q[18];
rz(pi/2) q[18];
rz(pi/2) q[20];
sx q[20];
rz(pi) q[20];
cz q[20], q[0];
sx q[20];
rz(pi/2) q[20];
c[0] = measure q[1];
c[1] = measure q[2];
c[2] = measure q[3];
c[3] = measure q[4];
c[4] = measure q[5];
c[5] = measure q[6];
c[6] = measure q[7];
c[7] = measure q[8];
c[8] = measure q[9];
c[9] = measure q[10];
c[10] = measure q[11];
c[11] = measure q[12];
c[12] = measure q[13];
c[13] = measure q[14];
c[14] = measure q[15];
c[15] = measure q[16];
c[16] = measure q[17];
c[17] = measure q[18];
c[18] = measure q[19];
c[19] = measure q[20];
