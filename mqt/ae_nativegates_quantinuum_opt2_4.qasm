// Benchmark created by MQT Bench on 2026-03-02
// For more info: https://www.cda.cit.tum.de/mqtbench/
// MQT Bench version: 2.1.0
// Qiskit version: 2.1.1
// Output format: qasm3
// Level: nativegates
// Target: quantinuum
// Used gateset: ['rx', 'ry', 'rz', 'rzz', 'reset', 'delay', 'measure']

OPENQASM 3.0;
include "stdgates.inc";
gate rzz(p0) _gate_q_0, _gate_q_1 {
  cx _gate_q_0, _gate_q_1;
  rz(p0) _gate_q_1;
  cx _gate_q_0, _gate_q_1;
}
bit[4] meas;
ry(pi/2) $0;
rz(1.1441688336680205) $0;
ry(-pi/2) $1;
rz(-1.3734007669450161) $1;
ry(pi/2) $2;
rz(-1.2566494638705523) $2;
rx(-pi/2) $3;
rzz(-0.9272952180016123) $0, $3;
rz(1.9974238199217718) $0;
ry(pi) $0;
rz(2.857798544381466) $3;
rzz(-1.2870022175865685) $1, $3;
rz(1.373400766945018) $1;
rz(1.2870022175865685) $3;
rzz(-0.5675882184166561) $2, $3;
rz(-1.884943189719241) $2;
ry(-pi/2) $2;
rzz(pi/4) $1, $2;
rz(3*pi/4) $1;
ry(pi/2) $1;
rz(-pi/4) $2;
rzz(pi/8) $0, $2;
rz(-pi/8) $0;
rzz(pi/4) $0, $1;
rz(3*pi/4) $0;
ry(pi/2) $0;
rz(-pi/4) $1;
rz(-pi/8) $2;
rz(1.0032081083782405) $3;
rx(pi/2) $3;
barrier $0, $1, $2, $3;
meas[0] = measure $0;
meas[1] = measure $1;
meas[2] = measure $2;
meas[3] = measure $3;
