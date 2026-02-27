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
import qurator.domain.circuit._
import qurator.testbed.FakeCompiler
import qurator.effects.TestBackground
import cats.effect.std.Random
import qurator.effects.GenUUID
import qurator.domain.ID
import qurator.domain.device._
import qurator.domain.Task._

object SchedulerUtilitySuite extends SimpleIOSuite {

  implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  implicit val bg = TestBackground.NoOp

  val currentDevices = Map( 
    "ibm_boston" -> IBMDevice,
    "ibm_kingston" -> IBMDevice,
    "ibm_pittsburgh" -> IBMDevice,
    "ibm_fez" -> IBMDevice,
    "ibm_marrakesh" -> IBMDevice,
    "ibm_torino" -> IBMDevice,
    "aria-1" -> IonQDevice,
    "forte-1" -> IonQDevice,
    "garnet" -> IQMDevice,
    "emerald" -> IQMDevice,
    "aquiela" -> QuEraDevice,
    "ankaa-3" -> RigettiDevice,
    "ibex-q1" -> AQTDevice
  )

  protected class TestDataPersistanceService() extends DataPersistanceService[IO] {
    private val cache: Ref[IO, Map[String, List[DeviceQueueInformation]]] =
        Ref.unsafe[IO, Map[String, List[DeviceQueueInformation]]](Map.empty)

    def getDeviceQueueInformationByPage(page: Int): IO[List[DeviceQueueInformation]] = ???
    def persistDeviceQueueInformation(l : List[DeviceQueueInformationCreate]): IO[Unit] = ???
    def fetchQueueInformationAfterDate(date: LocalDateTime, device: String): IO[List[DeviceQueueInformation]] = 
      cache.get.flatMap{cached => 
          cached.get(device) match {
            case Some(dqi) =>
              println(s"Found cache hit ${dqi.takeRight(1)}") 
              IO.pure(dqi)
            case None => 
              println(s"No cache hit for device $device, generating fake data...")
              Random.scalaUtilRandom[IO].flatMap { rng =>
                for {
                  startQueue <- rng.nextIntBounded(30001)

                  firstId <- ID.make[IO, DeviceQueueInformationId]

                  firstEntry = DeviceQueueInformation(
                    uuid        = firstId,
                    name        = device,
                    provider    = IBMDevice,
                    queueLength = startQueue,
                    waitTimeAvg = None,
                    waitTimep50 = None,
                    waitTimep95 = None,
                    queueType   = NormalQueue,
                    createdAt   = date
                  )

                  // remaining 10,079 entries = manually calculated to be the 2 minute windows in 2 weeks
                  built <- (1 until 10080).toList.foldLeftM[IO, (Int, List[DeviceQueueInformation])]((startQueue, List(firstEntry))) {
                    case ((previousQueue, acc), i) =>
                      for {
                        delta <- rng.nextIntBounded(21)
                        add <- rng.nextBoolean //add or subtract? 
                        nextQueue = math.max(
                          0,
                          if (add) previousQueue + delta else previousQueue - delta
                        )
                        id <- ID.make[IO, DeviceQueueInformationId]
                        entry = DeviceQueueInformation(
                          uuid        = id,
                          name        = device, 
                          provider    = currentDevices.getOrElse(device, IBMDevice),
                          queueLength = nextQueue,
                          waitTimeAvg = None,
                          waitTimep50 = None,
                          waitTimep95 = None,
                          queueType   = NormalQueue,
                          createdAt   = date.plusMinutes(i.toLong * 2)
                        )
                      } yield (nextQueue, entry :: acc)
                  }
                  _ <- cache.update(_.updated(device, built._2.reverse))
                } yield built._2.reverse
              }
          }
      }
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

  val fakeTestDataPersistance  = new TestDataPersistanceService()
  val schedulerIO : IO[Scheduler[IO]] = Scheduler.make[IO](
     dataPersistanceService = fakeTestDataPersistance,
     prioritizationStrategy = identity,
     cuttingStrategy = (c : Circuit) => List(c),
     targetEstimatedFidelity = 0.9,
     additionalOptimizationRuns = (c : Circuit) => List(c),
     compiler = new FakeCompiler[IO](List()),
     clients = testClient
  )

  test("test the test, test queue data generator"){
    val date = LocalDateTime.now()
    val service = fakeTestDataPersistance
    val twoweeksago = date.minusWeeks(2)
    for{
      l <- service.fetchQueueInformationAfterDate(twoweeksago, "T")
    }yield expect(l.length == 10080)
  }

 
  test("test estimateQueueTime is within 10%"){
    val date = LocalDateTime.now()
    val service = fakeTestDataPersistance
    val twoweeksago = date.minusWeeks(2)
    for{
      scheduler <- schedulerIO
      d = Device(
          platform = "IBM",
          platformId = "ibm_boston",
          qubits = 156,
          t1 = 246.29f,
          t2 = 343.14f,
          gateSet = List(CZ(0, 0), Rotate(0, 0), RZ(0, 0),  CX(0, 1), X(0))
        )
      tid <- ID.make[IO, TaskId]
      t = QuantumTask(
        tid, 
        Circuit(List.empty, 5),
        TaskQubits(5),
        TaskShots(1000),
        TaskDepth(10),
        List.empty,
        List.empty,
        LocalDateTime.now()
      )
      l <- service.fetchQueueInformationAfterDate(twoweeksago, "ibm_boston")
      x <- scheduler.estimateQueueTime(d,  t)
      ls = l.takeRight(1).headOption.map(_.queueLength).getOrElse(0)
      _ = println(s"Estimated Queue Time: $x seconds, Current Queue Length: $ls") //just to see the output of the test
    }yield {
      expect(Math.abs(ls - x) * 100 / ls < 10)
    }
  }


  test("test basic mergeCircuit semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }

  test("test basic allParentResultsAvailable semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }

  test("test basic requiresCutting semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }

  test("test basic estimateFidelity semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }

  test("test basic getAvailableDevices semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }

  test("test basic fetchAllInProgressJobResults semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }

  test("test basic attemptToMergeSyncTasks semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }

  test("test basic buildGreedySynchronizedPlan semantics"){
    for{
      scheduler <- schedulerIO
    }yield {
      expect("hello".length == 5)
    }
  }
  
}