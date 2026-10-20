import cats.effect.{ExitCode, IO, IOApp}
import qurator.testbed.BenchmarkDeviceRegistry
import qurator.testbed.fidelity.BenchmarkFidelityEvaluator
import qurator.util.{FidelityEstimator, Qasm3Parser}


object RunFidelityEvaluation extends IOApp {
  def run(args: List[String]): IO[ExitCode] = args match {
    case file :: deviceId :: Nil => for {
      evaluator <- IO(BenchmarkFidelityEvaluator.configured)
      raw <- IO.fromOption(BenchmarkDeviceRegistry.defaultCalibrations.get(deviceId))(
        new IllegalArgumentException(s"Unknown profile '$deviceId'. Available: " +
          BenchmarkDeviceRegistry.defaultCalibrations.keys.toVector.sorted.mkString(", ")))
      qasm <- IO.blocking(java.nio.file.Files.readString(java.nio.file.Paths.get(file)))
      circuit <- IO(Qasm3Parser.parse(qasm, file))
      calibration <- IO(FidelityEstimator.normalizeCalibration(raw))
      start <- IO.monotonic
      evaluation <- IO(evaluator.evaluate(circuit, calibration))
      finish <- IO.monotonic
      _ <- IO.println(s"device=$deviceId circuit=$file qubits=${circuit.qubits} gates=${circuit.remainingGates.size} " +
        s"evaluationMillis=${(finish - start).toMillis}")
      _ <- IO.println(evaluation)
    } yield ExitCode.Success
    case _ => IO.println("Usage: runMain RunFidelityEvaluation <compiled.qasm> <hard-coded-profile-id>")
      .as(ExitCode.Error)
  }
}
