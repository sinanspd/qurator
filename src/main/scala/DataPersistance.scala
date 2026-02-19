package qurator


import cats.effect._
import cats.effect.std.Supervisor
import eu.timepit.refined.auto._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import qurator.Config
import scala.language.postfixOps
import fs2.Stream
import scala.concurrent.duration._
import qurator.clients.IBMClient
import cats.effect.Temporal
import qurator.AppResources
import qurator.clients.BraketClient
import qurator.modules.Services
import qurator.modules.HttpApi
import qurator.resources.MkHttpServer


object DataPersitance extends IOApp.Simple {

  implicit val logger = Slf4jLogger.getLogger[IO]

   override def run: IO[Unit] = 
    Config.load[IO].flatMap { cfg =>
        Logger[IO].info(s"Loaded config: $cfg") *>
        Supervisor[IO].use { implicit sp =>
            AppResources
            .make[IO](cfg)
            .flatMap {res => 
                val services = Services.make[IO](res.postgres)
                val api      = HttpApi.make[IO](services)
                MkHttpServer[IO].newEmber(cfg.httpServerConfig, api.httpApp)
            }
            .useForever
        }
    }
}