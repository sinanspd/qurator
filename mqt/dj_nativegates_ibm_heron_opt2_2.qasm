// Benchmark created by MQT Bench on 2025-11-24
// For more info: https://www.cda.cit.tum.de/mqtbench/
// MQT Bench version: 2.1.0
// Qiskit version: 2.1.1
// Output format: qasm3
// Level: nativegates
// Target: ibm_heron
// Used gateset: ['id', 'x', 'sx', 'rz', 'cz', 'reset', 'delay', 'measure']

OPENQASM 3.0;
include "stdgates.inc";
bit[1] c;
qubit[2] q;
rz(-pi/2) q[0];
sx q[0];
rz(pi) q[0];
x q[1];
cz q[0], q[1];
sx q[0];
rz(-pi/2) q[0];
rz(pi/2) q[1];
sx q[1];
rz(pi/2) q[1];
barrier q[0], q[1];
c[0] = measure q[0];
