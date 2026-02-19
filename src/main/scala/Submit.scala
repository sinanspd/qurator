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
import qurator.clients.AzureQuantumClient
import qurator.domain.IBM.SubmitJobRequestV2
import qurator.testcircuits.BellPairTask
import scala.annotation.meta.param
import qurator.domain.IBM.SamplerV2Input
import qurator.domain.IBM.SamplerV2PUB


object Submitter extends IOApp.Simple {

  implicit val logger = Slf4jLogger.getLogger[IO]

   override def run: IO[Unit] = 
    Config.load[IO].flatMap { cfg =>
        Logger[IO].info(s"Loaded config: $cfg") *>
        Supervisor[IO].use { implicit sp =>
            AppResources
            .make[IO](cfg)
            .evalMap {res => 
                val c = IBMClient.make[IO](cfg.ibmCredentials, res.client)
                val a = BraketClient.make[IO](cfg.braketConfig, res.client)
                val z = AzureQuantumClient.make[IO](cfg.azureConfig, res.client)
                val r = SubmitJobRequestV2(
                    "sampler", 
                    "ibm_fez",
                    None, 
                    None,
                    None, 
                    None,
                    None,
                    None,
                    SamplerV2Input(
                        pubs = List(
                             BellPairTask.qasm
                            // SamplerV2PUB(
                            //     circuit = BellPairTask.qasm,
                            //     shots = Some(1024),
                            // )
                    ))
                )

                import org.http4s.circe._
                import io.circe.syntax._        
                println(r.asJson.spaces2)

                c.submitJob(r)
            }.useForever
        }
    }
}