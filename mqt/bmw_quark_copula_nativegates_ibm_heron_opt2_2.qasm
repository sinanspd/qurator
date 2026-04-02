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
qubit[2] q;
rz(pi/2) q[0];
sx q[0];
rz(pi/2) q[0];
rz(pi/2) q[1];
sx q[1];
rz(pi) q[1];
cz q[0], q[1];
sx q[1];
rz(pi/2) q[1];
barrier q[0], q[1];
rz(1.2943469151946534) q[0];
sx q[0];
rz(-1.8366893559320356) q[0];
sx q[0];
rz(0.49288375058031875) q[0];
rz(-0.632829084421374) q[1];
sx q[1];
rz(-0.08045377785829366) q[1];
sx q[1];
rz(-0.7167882678555841) q[1];
barrier q[0], q[1];
rz(2.758547559076651) q[0];
sx q[0];
rz(-2.1472642801609716) q[0];
sx q[0];
rz(1.1027555537727682) q[0];
rz(-1.8414382290182099) q[1];
sx q[1];
rz(-2.044126937338996) q[1];
sx q[1];
rz(0.5542731551896622) q[1];
barrier q[0], q[1];
barrier q[0], q[1];
meas[0] = measure q[0];
meas[1] = measure q[1];
