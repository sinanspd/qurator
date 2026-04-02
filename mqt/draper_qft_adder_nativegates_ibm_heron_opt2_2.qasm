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
bit[2] meas;
qubit[1] a;
qubit[1] b;
x a[0];
rz(pi/2) b[0];
sx b[0];
cz a[0], b[0];
x a[0];
sx b[0];
rz(pi/2) b[0];
barrier a[0], b[0];
meas[0] = measure a[0];
meas[1] = measure b[0];
