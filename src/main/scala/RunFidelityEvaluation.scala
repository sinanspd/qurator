import cats.effect.{ExitCode, IO, IOApp}
import qurator.testbed.BenchmarkDeviceRegistry
import qurator.testbed.fidelity.{BenchmarkFidelityEvaluator, DeviceFidelityEvaluation}
import qurator.util.Qasm3Parser


object RunFidelityEvaluation extends IOApp {
  def run(args: List[String]): IO[ExitCode] = args match {
    case file :: deviceId :: Nil => for {
      evaluator <- IO(BenchmarkFidelityEvaluator.configured)
      raw <- IO.fromOption(BenchmarkDeviceRegistry.defaultCalibrations.get(deviceId))(
        new IllegalArgumentException(s"Unknown profile '$deviceId'. Available: " +
          BenchmarkDeviceRegistry.defaultCalibrations.keys.toVector.sorted.mkString(", ")))
      device <- IO.fromOption(BenchmarkDeviceRegistry.defaultDevices.find(_.platformId == deviceId))(
        new IllegalArgumentException(s"Profile '$deviceId' has no enabled gate-based benchmark device"))
      qasm <- IO.blocking(java.nio.file.Files.readString(java.nio.file.Paths.get(file)))
      circuit <- IO(Qasm3Parser.parse(qasm, file))
      result <- DeviceFidelityEvaluation.run(circuit, device, raw, evaluator)
      _ <- IO.println(s"device=$deviceId circuit=$file compiler=qurator_topology_aware " +
        s"inputQubits=${circuit.qubits} inputGates=${circuit.remainingGates.size} " +
        s"compiledRegisterQubits=${result.compiledCircuit.qubits} physicalQubitsUsed=${result.physicalQubitsUsed} " +
        s"compiledGates=${result.compiledCircuit.remainingGates.size} " +
        s"compilationMillis=${result.compilationMillis} evaluationMillis=${result.evaluationMillis}")
      _ <- IO.println(result.evaluation)
    } yield ExitCode.Success
    case _ => IO.println("Usage: runMain RunFidelityEvaluation <circuit.qasm> <hard-coded-profile-id>")
      .as(ExitCode.Error)
  }
}
