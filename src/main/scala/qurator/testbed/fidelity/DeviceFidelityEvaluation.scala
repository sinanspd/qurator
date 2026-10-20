package qurator.testbed.fidelity

import cats.effect.IO
import qurator.domain.calibration.{DeviceCalibration, gateKey}
import qurator.domain.circuit.Circuit
import qurator.domain.device.Device
import qurator.testbed.{FakeCompiler, HaqaMapper}
import qurator.util.FidelityEstimator

/** Offline placement/routing and evaluation using the same compiler as RunBenchmarks. */
object DeviceFidelityEvaluation {
  final case class Result(
      compiledCircuit: Circuit,
      evaluation: FidelityEvaluation,
      compilationMillis: Long,
      evaluationMillis: Long
  ) {
    def physicalQubitsUsed: Int =
      compiledCircuit.remainingGates.iterator.flatMap(gateKey(_)._2).toSet.size
  }

  def run(
      circuit: Circuit,
      device: Device,
      raw: DeviceCalibration,
      evaluator: BenchmarkFidelityEvaluator
  ): IO[Result] = for {
    start <- IO.monotonic
    compiled <- compile(circuit, device, raw)
    compiledAt <- IO.monotonic
    evaluation <- IO(evaluator.evaluate(compiled, FidelityEstimator.normalizeCalibration(raw)))
    evaluatedAt <- IO.monotonic
  } yield Result(compiled, evaluation, (compiledAt - start).toMillis, (evaluatedAt - compiledAt).toMillis)

  /** Compile once for shared policy inputs; evaluate only after policies have chosen devices. */
  def compile(circuit: Circuit, device: Device, raw: DeviceCalibration): IO[Circuit] = for {
    topology <- IO.fromOption(HaqaMapper.topologyFromCalibration(raw).filter(_.qubits.nonEmpty))(
      new IllegalArgumentException(
        s"Device '${device.platformId}' has no calibration topology; cannot place and route the circuit. " +
          "Supply a topology-bearing calibration instead of evaluating the raw circuit."))
    _ <- IO {
      require(circuit.qubits > 0 && circuit.qubits <= device.qubits &&
        circuit.qubits <= topology.qubits.size,
        s"Circuit needs ${circuit.qubits} qubits, but '${device.platformId}' has " +
          s"${device.qubits} device qubits and ${topology.qubits.size} calibrated topology qubits")
      require(circuit.remainingGates.iterator.flatMap(gateKey(_)._2)
        .forall(q => q >= 0 && q < circuit.qubits), "Circuit operand outside its declared register")
    }
    compiler <- IO(FakeCompiler.topologyAwareFromCalibrations[IO](Map(device.platformId -> raw)))
    compiled <- IO.defer(compiler.compileCircuitFor(device, circuit))
    // The benchmark compiler has permissive fallbacks. Do not score an unrouted
    // interaction or an unmapped physical qubit as if compilation had succeeded.
    _ <- IO {
      val physical = topology.qubits.toSet
      compiled.remainingGates.foreach { gate =>
        val (kind, qubits) = gateKey(gate)
        require(qubits.forall(physical.contains),
          s"Compiler emitted an uncalibrated physical qubit for '${device.platformId}': $gate")
        if (kind != "BARRIER" && qubits.size == 2) {
          require(qubits.distinct.size == 2 && topology.hasEdge(qubits(0), qubits(1)),
            s"Compiler could not route an interaction on '${device.platformId}': $gate")
        }
        require(kind == "BARRIER" || qubits.size <= 2,
          s"Compiler left a multi-qubit operation without topology-validated lowering: $gate")
      }
    }
  } yield compiled
}
