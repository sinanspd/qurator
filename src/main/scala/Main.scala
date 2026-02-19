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
import qurator.domain.IBM
import qurator.modules.Services
import qurator.domain.Braket
import qurator.domain.Azure
import qurator.clients.AzureQuantumClient


object Main extends IOApp.Simple {

  implicit val logger = Slf4jLogger.getLogger[IO]

   override def run: IO[Unit] =
    Config.load[IO].flatMap { cfg =>
        Logger[IO].info(s"Loaded config: $cfg") *>
        Supervisor[IO].use { implicit sp =>
            println("t1")
            AppResources
            .make[IO](cfg)
            .flatMap { res => 
              println("Test that I am here")
              val c = IBMClient.make[IO](cfg.ibmCredentials, res.client)
              val a = BraketClient.make[IO](cfg.braketConfig, res.client)
              val z = AzureQuantumClient.make[IO](cfg.azureConfig, res.client)
              val persistanceService = Services.make[IO](res.postgres).dataPersistanceService
              Stream
                .fixedRateStartImmediately[IO](2.minutes)
                .evalTap(_ => 
                  for {
                    _ <- Logger[IO].info(s"Before fetch")
                    deviceinfo <- c.fetchDeviceInformation
                    _ <- Logger[IO].info(s"Fetched IBM device info: $deviceinfo")
                    ibmQueueInfo = IBM.toDeviceQueueInformation(deviceinfo.devices)
                    _ <- persistanceService.persistDeviceQueueInformation(ibmQueueInfo)
                    _ <- Logger[IO].info(s"Persisted IBM queue info")
                    braketDevices <- a.fetchDeviceList
                    brakedDeviceIds = braketDevices.devices.map(_.deviceArn)
                    bc <- a.fetchDeviceDetails(List(brakedDeviceIds(0)))
                    _ <- Logger[IO].info(s"Fetched Braket devices: ${bc.map(d => (d.deviceArn, d.deviceStatus, d.deviceQueueInfo))}")
                    braketDeviceQueueInfo = Braket.toDeviceQueueInformation(bc)
                    _ <- persistanceService.persistDeviceQueueInformation(braketDeviceQueueInfo)
                     _ <- Logger[IO].info(s"Persisted Braket queue info")
                    azureDevices <- z.fetchDeviceInformation
                    _ <- Logger[IO].info(s"Fetched Azure devices: $azureDevices")
                    azureDeviceInfo = Azure.toDeviceQueueInformation(azureDevices.value)
                    _ <- persistanceService.persistDeviceQueueInformation(azureDeviceInfo)
                    _ <- Logger[IO].info(s"Persisted Azure queue info")
                  } yield ()
                )
                .compile.drain.background
            }.useForever
        }
    }
}