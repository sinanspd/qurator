// Benchmark created by MQT Bench on 2025-12-08
// For more info: https://www.cda.cit.tum.de/mqtbench/
// MQT Bench version: 2.1.0
// Qiskit version: 2.1.1
// Output format: qasm3
// Level: nativegates
// Target: ibm_heron
// Used gateset: ['id', 'x', 'sx', 'rz', 'cz', 'reset', 'delay', 'measure']

OPENQASM 3.0;
include "stdgates.inc";
bit[8] c;
qubit[9] q;
rz(-pi/2) q[0];
sx q[0];
rz(-pi) q[0];
rz(-pi/2) q[1];
sx q[1];
rz(pi) q[1];
rz(pi/2) q[2];
sx q[2];
rz(pi) q[2];
rz(pi/2) q[3];
sx q[3];
rz(pi) q[3];
rz(-pi/2) q[4];
sx q[4];
rz(pi) q[4];
rz(-pi/2) q[5];
sx q[5];
rz(pi) q[5];
rz(-pi/2) q[6];
sx q[6];
rz(pi) q[6];
rz(pi/2) q[7];
sx q[7];
rz(pi) q[7];
x q[8];
cz q[0], q[8];
sx q[0];
rz(-pi/2) q[0];
cz q[1], q[8];
sx q[1];
rz(-pi/2) q[1];
cz q[2], q[8];
sx q[2];
rz(pi/2) q[2];
cz q[3], q[8];
sx q[3];
rz(pi/2) q[3];
cz q[4], q[8];
sx q[4];
rz(-pi/2) q[4];
cz q[5], q[8];
sx q[5];
rz(-pi/2) q[5];
cz q[6], q[8];
sx q[6];
rz(-pi/2) q[6];
cz q[7], q[8];
sx q[7];
rz(pi/2) q[7];
rz(pi/2) q[8];
sx q[8];
rz(pi/2) q[8];
barrier q[0], q[1], q[2], q[3], q[4], q[5], q[6], q[7], q[8];
c[0] = measure q[0];
c[1] = measure q[1];
c[2] = measure q[2];
c[3] = measure q[3];
c[4] = measure q[4];
c[5] = measure q[5];
c[6] = measure q[6];
c[7] = measure q[7];
