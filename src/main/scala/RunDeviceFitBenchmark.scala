import cats.effect._
import cats.syntax.all._
import fs2.io.file.Path
import org.http4s.circe._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import qurator.clients.{BraketClient, IBMClient}
import qurator.domain.ProviderClient
import qurator.{Config, MkHttpClient}
import qurator.testbed.DeviceFitBenchmark

import java.nio.file.Files

object RunDeviceFitBenchmark extends IOApp {

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def run(args: List[String]): IO[ExitCode] =
    for {
      folder <- resolveQasmFolder(args.headOption)
      output <- resolveOutputPath(args.lift(1))
      cfg <- Config.load[IO]
      _ <- Logger[IO].info(s"Running device fit benchmark with QASM folder: $folder")
      report <- MkHttpClient[IO].newEmber(cfg.httpClientConfig).use { client =>
        val clients: List[ProviderClient[IO]] =
          List(
            IBMClient.make[IO](cfg.ibmCredentials, client),
            BraketClient.make[IO](cfg.braketConfig, client)
          )

        DeviceFitBenchmark.run(
          clients = clients,
          settings = DeviceFitBenchmark.Settings(qasmFolder = folder)
        )
      }
      _ <- report.parseWarnings.traverse_ { warning =>
        Logger[IO].warn(s"${warning.file}: ${warning.message}")
      }
      _ <- DeviceFitBenchmark.writeCsv(report, output)
      _ <- IO.println(s"Wrote ${report.rows.size} device fit benchmark rows to $output")
    } yield ExitCode.Success

  private def resolveQasmFolder(arg: Option[String]): IO[Path] =
    IO.delay {
      val candidates =
        arg.toList ++
          sys.env.get("QURATOR_DEVICE_FIT_QASM_FOLDER").toList ++
          List("mqtext", "mqt")

      candidates
        .map(raw => Path(raw))
        .find(path => Files.isDirectory(java.nio.file.Paths.get(path.toString)))
        .getOrElse(Path(candidates.headOption.getOrElse("mqt")))
    }

  private def resolveOutputPath(arg: Option[String]): IO[Path] =
    IO.delay {
      Path(
        arg
          .orElse(sys.env.get("QURATOR_DEVICE_FIT_OUTPUT"))
          .getOrElse("device_fit_benchmark.csv")
      )
    }
}
