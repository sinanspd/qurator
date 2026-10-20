import cats.effect.{ExitCode, IO, IOApp}
import java.nio.file.Paths
import qurator.testbed.QuantumPolicyBenchmark
import qurator.testbed.QuantumPolicyBenchmark.Settings

object RunQuantumPolicyBenchmark extends IOApp {
  // Edit these values to configure the benchmark.
  private val taskCount = 5
  private val maxQubits = 5
  private val seed = 42L
  private val circuitFolder = "mqt"
  private val qosSource = "/tmp/qurator-qos-artifacts/scheduler.py"
  private val pythonExecutable = "/tmp/qurator-qos-artifacts/venv/bin/python"
  private val targetEstimatedFidelity = 0.9

  def run(args: List[String]): IO[ExitCode] =
    (for {
      config <- IO {
        require(args.isEmpty, "Edit the configuration values in RunQuantumPolicyBenchmark.scala and run without arguments")
        Settings(
          tasks = taskCount,
          maxQubits = maxQubits,
          seed = seed,
          folder = Paths.get(circuitFolder),
          qosSource = Paths.get(qosSource),
          python = pythonExecutable,
          targetEstimatedFidelity = targetEstimatedFidelity
        )
      }
      summaries <- QuantumPolicyBenchmark.run(config)
      _ <- IO.println("policy,mean_fragment_fidelity_proxy,mean_fragment_queue_seconds")
      _ <- summaries.foldLeft(IO.unit) { (acc, row) =>
        acc *> IO.println(String.format(java.util.Locale.ROOT, "%s,%.6f,%.6f",
          row.policy, Double.box(row.overallFidelity), Double.box(row.meanQueueSeconds)))
      }
    } yield ExitCode.Success).handleErrorWith { e =>
      IO.println(s"Quantum policy benchmark failed: ${e.getMessage}").as(ExitCode.Error)
    }
}
