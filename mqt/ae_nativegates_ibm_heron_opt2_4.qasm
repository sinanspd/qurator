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
rz(pi/2) $0;
sx $0;
rz(-2.677945044588987) $0;
rz(-pi/2) $1;
sx $1;
rz(-1.6423823735618246) $1;
sx $1;
rz(pi/2) $2;
sx $2;
rz(-0.23776914631355517) $2;
sx $2;
x $3;
rz(-pi) $3;
cz $0, $3;
x $0;
rz(-pi/2) $0;
sx $3;
rz(2.214297435588181) $3;
sx $3;
cz $0, $3;
x $0;
rz(3.070644126287709) $0;
rz(-pi) $3;
sx $3;
rz(-1.8545904360032246) $3;
cz $3, $1;
sx $1;
rz(1.8545904360032246) $1;
sx $1;
x $3;
rz(-pi/2) $3;
cz $3, $1;
sx $1;
rz(2.2846084434254177) $1;
sx $1;
x $3;
rz(-0.28379410920832804) $3;
cz $3, $2;
sx $2;
rz(2.574004435173137) $2;
sx $2;
x $3;
rz(-2.5740044351731393) $3;
cz $3, $2;
sx $2;
rz(2.903823507276239) $2;
sx $2;
rz(-3*pi/4) $2;
cz $2, $1;
sx $1;
rz(3*pi/4) $1;
sx $1;
x $2;
rz(-3*pi/2) $2;
cz $2, $1;
rz(-pi) $1;
sx $1;
sx $2;
cz $0, $2;
sx $2;
rz(-7*pi/8) $2;
sx $2;
rz(pi) $2;
cz $0, $2;
cz $0, $1;
rz(-pi/2) $1;
sx $1;
rz(3*pi/4) $1;
sx $1;
cz $0, $1;
sx $0;
rz(pi/2) $0;
sx $1;
rz(pi/4) $1;
sx $2;
rz(3*pi/8) $2;
sx $3;
rz(-pi) $3;
barrier $0, $1, $2, $3;
meas[0] = measure $0;
meas[1] = measure $1;
meas[2] = measure $2;
meas[3] = measure $3;
