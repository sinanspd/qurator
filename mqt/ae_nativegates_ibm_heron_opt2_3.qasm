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
bit[3] meas;
rz(pi/2) $0;
sx $0;
rz(0.9664016777230966) $0;
sx $0;
rz(-pi/2) $1;
sx $1;
rz(-3*pi/4) $1;
rz(-pi) $2;
sx $2;
rz(-pi/2) $2;
cz $2, $0;
sx $0;
rz(2.214297435588181) $0;
sx $0;
x $2;
rz(-1.2870022175865685) $2;
cz $2, $0;
sx $0;
rz(2.537198004517993) $0;
sx $0;
sx $2;
rz(-pi) $2;
cz $1, $2;
x $1;
rz(-pi/2) $1;
sx $2;
rz(1.8545904360032246) $2;
sx $2;
cz $1, $2;
x $1;
rz(-5*pi/4) $1;
cz $0, $1;
sx $0;
sx $1;
cz $0, $1;
rz(-pi/2) $0;
sx $0;
rz(-3*pi/4) $0;
sx $0;
sx $1;
cz $0, $1;
rz(-pi) $0;
sx $0;
rz(-pi/4) $0;
rz(3*pi/4) $1;
sx $1;
rz(-pi/2) $1;
rz(-pi) $2;
sx $2;
rz(2.857798544381465) $2;
sx $2;
barrier $1, $0, $2;
meas[0] = measure $1;
meas[1] = measure $0;
meas[2] = measure $2;
