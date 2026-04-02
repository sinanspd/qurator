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
bit[3] meas;
ry(-pi/2) $0;
rz(1.9126184240646076) $0;
ry(-pi/2) $1;
rz(-2.1487894479894036) $1;
rx(pi/2) $2;
rzz(-0.9272952180016123) $2, $0;
rz(-2.9246316463487654) $0;
ry(-pi/2) $0;
rz(0.2837941092083276) $2;
ry(pi) $2;
rzz(-1.2870022175865685) $2, $1;
rz(-0.9928032056003895) $1;
rx(-0.2429514808112656) $1;
rzz(-pi/2) $0, $1;
rx(-pi/2) $0;
rz(-pi) $0;
rx(-pi/2) $1;
rz(-pi) $1;
rzz(-pi/2) $0, $1;
ry(-pi/2) $0;
rz(-pi/2) $0;
ry(-pi/2) $1;
rz(pi/2) $1;
rzz(-pi/4) $0, $1;
rz(2.113243009381079) $0;
rz(0.22661505888670774) $1;
ry(pi/2) $1;
rz(2.857798544381465) $2;
rx(pi/2) $2;
barrier $1, $0, $2;
meas[0] = measure $1;
meas[1] = measure $0;
meas[2] = measure $2;
