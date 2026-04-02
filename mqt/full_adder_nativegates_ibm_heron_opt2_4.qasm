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
bit[4] meas;
qubit[4] q;
rz(pi/2) q[3];
cz q[2], q[3];
sx q[3];
rz(3*pi/4) q[3];
sx q[3];
rz(pi) q[3];
cz q[1], q[3];
sx q[3];
rz(5*pi/4) q[3];
sx q[3];
rz(pi) q[3];
cz q[2], q[3];
rz(-pi/4) q[2];
sx q[2];
sx q[3];
rz(3*pi/4) q[3];
sx q[3];
cz q[1], q[3];
cz q[1], q[2];
rz(-3*pi/4) q[1];
rz(-pi) q[2];
sx q[2];
rz(-pi) q[2];
sx q[3];
rz(3*pi/4) q[3];
sx q[3];
cz q[2], q[3];
sx q[3];
rz(3*pi/4) q[3];
sx q[3];
rz(pi) q[3];
cz q[0], q[3];
sx q[3];
rz(5*pi/4) q[3];
sx q[3];
rz(pi) q[3];
cz q[2], q[3];
sx q[2];
sx q[3];
rz(3*pi/4) q[3];
sx q[3];
rz(pi) q[3];
cz q[0], q[3];
cz q[0], q[2];
rz(-3*pi/4) q[0];
rz(-pi) q[2];
sx q[2];
rz(-3*pi/4) q[2];
sx q[3];
rz(5*pi/4) q[3];
sx q[3];
rz(pi/2) q[3];
barrier q[0], q[1], q[2], q[3];
meas[0] = measure q[0];
meas[1] = measure q[1];
meas[2] = measure q[2];
meas[3] = measure q[3];
