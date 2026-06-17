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
      cfg <- Config.load[IO]
      folder <- resolveQasmFolder(args.headOption, cfg.deviceFitConfig.qasmFolder.map(_.value))
      output <- resolveOutputPath(args.lift(1), cfg.deviceFitConfig.output.map(_.value))
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
      _ <- Logger[IO].info(
        s"Device fit benchmark loaded ${report.loadedCircuits} circuits and ${report.targets.size} usable device targets"
      )
      _ <-
        if (report.targets.isEmpty)
          Logger[IO].warn("No usable device targets were discovered; all benchmark fidelity rows will be undefined")
        else IO.unit
      _ <-
        if (report.rows.nonEmpty && report.rows.forall(_.accommodatingDevices == 0))
          Logger[IO].warn("Every benchmark row has zero accommodating devices; check preceding target/failure warnings")
        else IO.unit
      _ <- report.parseWarnings.traverse_ { warning =>
        Logger[IO].warn(s"${warning.file}: ${warning.message}")
      }
      _ <- DeviceFitBenchmark.writeCsv(report, output)
      _ <- IO.println(s"Wrote ${report.rows.size} device fit benchmark rows to $output")
    } yield ExitCode.Success

  private def resolveQasmFolder(arg: Option[String], configured: Option[String]): IO[Path] =
    IO.delay {
      val candidates =
        arg.toList ++
          configured.toList ++
          List("mqtext", "mqt")

      candidates
        .map(raw => Path(raw))
        .find(path => Files.isDirectory(java.nio.file.Paths.get(path.toString)))
        .getOrElse(Path(candidates.headOption.getOrElse("mqt")))
    }

  private def resolveOutputPath(arg: Option[String], configured: Option[String]): IO[Path] =
    IO.delay {
      Path(
        arg
          .orElse(configured)
          .getOrElse("device_fit_benchmark.csv")
      )
    }
}
