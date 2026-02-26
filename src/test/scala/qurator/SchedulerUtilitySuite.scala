package qurator

import weaver.SimpleIOSuite
import cats.effect._
import qurator.programs.Scheduler
import qurator.service.DataPersistanceService
import qurator.domain.DeviceQueueInformation._
import java.time.LocalDateTime
import qurator.modules.HttpClients
import qurator.Types.AppConfig
import qurator.Types._
import qurator.domain.IBM.IBMConfig
import qurator.domain.Braket.BraketConfig
import qurator.domain.Azure.AzureConfig
import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.cats._
import derevo.cats._
import cats.syntax.all._
import ciris._
import ciris.refined._
import com.comcast.ip4s._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.net.UserPortNumber
import eu.timepit.refined.types.numeric.PosInt
import ciris.Secret
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{ HttpRoutes, Response }
import qurator.domain.circuit.Circuit
import qurator.testbed.FakeCompiler
import qurator.effects.TestBackground

object SchedulerUtilitySuite extends SimpleIOSuite {

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  implicit val bg = TestBackground.NoOp

  protected class TestDataPersistanceService() extends DataPersistanceService[IO] {
    def getDeviceQueueInformationByPage(page: Int): IO[List[DeviceQueueInformation]] = ???
    def persistDeviceQueueInformation(l : List[DeviceQueueInformationCreate]): IO[Unit] = ???
    def fetchQueueInformationAfterDate(date: LocalDateTime, device: String): IO[List[DeviceQueueInformation]] = ???
    def getQueueMinAfterDateForDevice(date: LocalDateTime, device: String): IO[Option[DeviceQueueInformation]] = ???
    def getQueueMaxAfterDateForDevice(date: LocalDateTime, device: String): IO[Option[DeviceQueueInformation]] = ???
  }


  def routes(mkResponse: IO[Response[IO]]) =
    HttpRoutes
      .of[IO] {
        case GET -> Root / "test" => mkResponse
      }
      .orNotFound


  val testClient = HttpClients.make[IO](
    cfg = AppConfig(
      ibmCredentials = IBMConfig(NonEmptyString("x"), Secret(NonEmptyString("x"))),
      braketConfig = BraketConfig(NonEmptyString("x"), Secret(NonEmptyString("x"))),
      azureConfig = AzureConfig(NonEmptyString("x"), NonEmptyString("x"), NonEmptyString("x"), Secret(NonEmptyString("x"))),
      postgreSQL = PostgreSQLConfig(
        host = NonEmptyString("localhost"),
        port = UserPortNumber(5432),
        user = NonEmptyString("user"),
        password = Secret(NonEmptyString("password")),
        database = NonEmptyString("db"),
        max = PosInt(10)  
     ),
     httpServerConfig = HttpServerConfig(
      host = Host.fromString("localhost").get,
      port = Port.fromInt(8080).get
     ),
     httpClientConfig = HttpClientConfig(
      timeout = scala.concurrent.duration.DurationInt(30).seconds,
      idleTimeInPool = scala.concurrent.duration.DurationInt(60).seconds
     )
    ),
    client = Client.fromHttpApp(routes(Ok()))
  )


  val scheduler : IO[Scheduler[IO]] = Scheduler.make[IO](
     dataPersistanceService = new TestDataPersistanceService(),
     prioritizationStrategy = identity,
     cuttingStrategy = (c : Circuit) => List(c),
     targetEstimatedFidelity = 0.9,
     additionalOptimizationRuns = (c : Circuit) => List(c),
     compiler = new FakeCompiler[IO](List()),
     clients = testClient
  )

 
  pureTest("non-effectful (pure) test"){
    expect("hello".size == 5)
  }
}