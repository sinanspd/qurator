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
import qurator.clients._
import qurator.domain.IBM._
import qurator.domain.calibration._
import org.http4s.client._
import org.http4s._ 
import qurator.domain.device._
import scala.annotation.nowarn
import qurator.domain.Braket._
import qurator.domain.Azure._
import qurator.testbed.FakeCompiler
import qurator.domain.CutQC.CutQCConfig
import qurator.util.CuttingStrategies

@nowarn
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
        case req @ GET -> Root / "api" / "v1" / "jobs" / id
            if req.uri.authority.exists(_.host.value == "quantum.cloud.ibm.com") =>
          mkResponse
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
     ),
     cutqcConfig = CutQCConfig(
      baseUri = NonEmptyString("http://localhost:8000")
     )
    ),
    client = Client.fromHttpApp(routes(Ok()))
  )

  val fakeTestDataPersistance  = new TestDataPersistanceService()
  val schedulerIO : IO[Scheduler[IO]] = Scheduler.make[IO](
     dataPersistanceService = fakeTestDataPersistance,
     prioritizationStrategy = identity,
     cuttingStrategy = CuttingStrategies.none[IO],
     targetEstimatedFidelity = 0.9,
     additionalOptimizationRuns = (c : Circuit) => List(c),
     compiler = new FakeCompiler[IO](List()),
     clients = testClient
  )

  private def ids(n: Int): IO[List[TaskId]] =
    List.fill(n)(()).traverse(_ => ID.make[IO, TaskId])

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
          gateSet = List(CZ(0, 0), RX("2", 0), RZ("3", 0),  CX(0, 1), X(0))
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

  test("classical task with no parents is ready") {
    for {
      taskId <- ID.make[IO, TaskId]
      task = ClassicalTask(
        uuid = taskId,
        program = "do something",
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      actual = Scheduler.allParentResultsAvailable(
        results = Map.empty,
        t = task
      )
    } yield expect(actual)
  }

  test("classical task is not ready when one parent is missing") {
    for {
      List(taskId, p1, p2) <- ids(3) 
      task = ClassicalTask(
        uuid = taskId,
        program = "do something",
        parentTasks = List(p1, p2),
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      actual = Scheduler.allParentResultsAvailable(
        results = Map(p1 -> ("done", 1L)),
        t = task
      )
    } yield expect(!actual)
  }

  test("classical task is ready when all parent results exist") {
    for {
      List(taskId, p1, p2) <- ids(3)
      task =  ClassicalTask(
        uuid = taskId,
        program = "do something",
        parentTasks = List(p1, p2),
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      actual = Scheduler.allParentResultsAvailable(
        results = Map(
          p1 -> ("done-1", 1L),
          p2 -> ("done-2", 1L)
        ),
        t = task
      )
    } yield expect(actual)
  }

  test("quantum task with no parents is ready") {
    for {
      taskId <- ID.make[IO, TaskId]
      task = QuantumTask(
        uuid = taskId,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      actual = Scheduler.allParentResultsAvailable(
        results = Map.empty,
        t = task
      )
    } yield expect(actual)
  }

  test("quantum task is not ready when a parent is missing") {
    for {
      List(taskId, p1, p2) <- ids(3)
      task = QuantumTask(
        uuid = taskId,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List(p1, p2),
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      actual = Scheduler.allParentResultsAvailable(
        results = Map(p2 -> ("done", 1L)),
        t = task
      )
    } yield expect(!actual)
  }

  test("quantum task is ready when all parent results exist") {
    for {
      List(taskId, p1, p2, p3) <- ids(4)
      task = QuantumTask(
        uuid = taskId,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List(p1, p2),
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      actual = Scheduler.allParentResultsAvailable(
        results = Map(
          p1 -> ("done-1", 1L),
          p2 -> ("done-2", 2L),
          p3 -> ("done-3", 3L)
        ),
        t = task
      )
    } yield expect(actual)
  }


  test("synchronized group is ready only when every child is ready") {
    for {
      List(sid, q1Id, q2Id, p1, p2, p3) <- ids(6)

      q1 = QuantumTask(
        uuid = q1Id,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List(p1, p2),
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      q2 = QuantumTask(
        uuid = q2Id,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List(p3),
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      group = SyncronizedQuantumTaskList(
        uuid = sid,
        tasks = List(q1, q2),
        t1Budget = 1000000L,
        createdAt = LocalDateTime.now()
      )

      notReady = Scheduler.allParentResultsAvailable(
        results = Map(
          p1 -> ("done-1", 1L),
          p2 -> ("done-2", 2L)
        ),
        t = group
      )

      ready = Scheduler.allParentResultsAvailable(
        results = Map(
          p1 -> ("done-1", 1L),
          p2 -> ("done-2", 2L),
          p3 -> ("done-3", 3L)
        ),
        t = group
      )
    } yield expect.all(
      !notReady,
      ready
    )
  }

  private def depths(buckets: List[List[QuantumTask]]): List[List[Int]] =
    buckets.map(_.map(_.depth.value))

  test("bucketByDepth returns a single singleton bucket for one task") {
    for{
      id <- ID.make[IO, TaskId]
      t = QuantumTask(
        uuid = id,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(7),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t), depthRelTol = 0.1)
    } yield {
       expect(depths(out) == List(List(7)))
    } 
  }

  test("bucketByDepth sorts input by depth before bucketing") {
    for{
      List(id1, id2, id3) <- ids(3)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(3),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(1),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(2),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t3, t1, t2), depthRelTol = 10.0)
    }yield {
      expect(depths(out) == List(List(1, 2, 3)))          
    }
  }


  test("bucketByDepth places all tasks in one bucket when all are within tolerance") {
    for{
      List(id1, id2, id3) <- ids(3)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(11), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(12), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t1, t2, t3), depthRelTol = 0.2)
    }yield {
      expect(depths(out) == List(List(10, 11, 12)))
    }
  }

  test("bucketByDepth splits tasks into multiple buckets when gaps exceed tolerance") {
    for{
      List(id1, id2, id3, id4, id5) <- ids(5)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(11), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(21), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t5 = QuantumTask(
        uuid = id5,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(40), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t1, t2, t3, t4, t5), depthRelTol = 0.05)
    } yield {
      expect(depths(out) == List(List(10), List(11), List(20, 21), List(40)))
    }
  }

  test("bucketByDepth includes a task exactly on the tolerance boundary") {
    // mean = 10, candidate = 12, relative diff = 2 / 10 = 0.2
    for{
      List(id1, id2) <- ids(2)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(800),
        depth = TaskDepth(12), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t1, t2), depthRelTol = 0.2)
    } yield {
       expect(depths(out) == List(List(10, 12)))
    }
  }

  test("bucketByDepth excludes a task just outside the tolerance boundary") {
    // mean = 10, candidate = 13, relative diff = 3 / 10 = 0.3
    for{
      List(id1, id2) <- ids(2)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(800),
        depth = TaskDepth(13), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t1, t2), depthRelTol = 0.2)
    } yield { expect(depths(out) == List(List(10), List(13)))}
  }

  test("bucketByDepth uses running mean of the current bucket") {
    // - tol = 0.20:
    // - 10 starts bucket
    // - 11 joins (|11 - 10| / 10 = 0.10)
    //   new mean = 10.5
    // - 12 joins because it is checked against 10.5:
    //   |12 - 10.5| / 10.5 = ~0.142857 <= 0.20
    for{
      List(id1, id2, id3) <- ids(3)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(800),
        depth = TaskDepth(11), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(800),
        depth = TaskDepth(12), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t1, t2, t3), depthRelTol = 0.20)
    } yield { expect(depths(out) == List(List(10, 11, 12))) }
  }

  test("bucketByDepth preserves ascending order within each bucket and across buckets") {
    for{
      List(id1, id2, id3, id4, id5) <- ids(5)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(21),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(40), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(11), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t5 = QuantumTask(
        uuid = id5,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20), 
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
       )
      out = Scheduler.bucketByDepth(List(t1, t2, t3, t4, t5), depthRelTol = 0.1)
    } yield {
      expect(depths(out) == List(List(10, 11), List(20, 21), List(40)))
    }
  }

  test("bucketByDepth with zero tolerance groups only equal depths") {
    for{
      List(id1, id2, id3, id4, id5) <- ids(5)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(5),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(5),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(6),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(6),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t5 = QuantumTask(
        uuid = id5,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(7),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.bucketByDepth(List(t1, t2, t3, t4, t5), depthRelTol = 0.0)
    } yield {
      expect(depths(out) == List(List(5, 5), List(6, 6), List(7)))
    }
  }

  private def mkDevice(qubits: Int, platformId: String): Device =
    Device(
      platform = "IBM",
      platformId = platformId,
      qubits = qubits,
      t1 = 0f,
      t2 = 0f,
      gateSet = List.empty
    )

  private def binQubits(bins: List[List[QuantumTask]]): List[List[Int]] =
    bins.map(_.map(_.qubits.value))

  private def allBinSumsLeq(bins: List[List[QuantumTask]], capacity: Int): Boolean =
    bins.forall(bin => bin.map(_.qubits.value).sum <= capacity)

  test("assignToFinalBuckets returns a single singleton bin for one task") {
    for {
      List(id1) <- ids(1)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1),
        capacity = 10,
        maxTasksPerBin = 3
      )
    } yield {
      expect(binQubits(out) == List(List(5)))
    }
  }

  test("assignToFinalBuckets sorts by descending qubit count before packing") {
    for {
      List(id1, id2, id3, id4) <- ids(4)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(3),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(4),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(6),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(7),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3, t4),
        capacity = 10,
        maxTasksPerBin = 3
      )
    } yield {
      expect(binQubits(out) == List(List(7, 3), List(6, 4)))
    }
  }

  test("assignToFinalBuckets allows an exact fit up to capacity") {
    for {
      List(id1, id2) <- ids(2)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(7),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(3),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2),
        capacity = 10,
        maxTasksPerBin = 3
      )
    } yield {
      expect(binQubits(out) == List(List(7, 3)))
    }
  }

  test("assignToFinalBuckets never exceeds capacity when each task individually fits") {
    for {
      List(id1, id2, id3, id4) <- ids(4)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(4),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(3),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3, t4),
        capacity = 7,
        maxTasksPerBin = 3
      )
    } yield {
      expect(binQubits(out) == List(List(5, 2), List(4, 3))) and
      expect(allBinSumsLeq(out, 7))
    }
  }

  test("assignToFinalBuckets respects maxTasksPerBin even when capacity allows more") {
    for {
      List(id1, id2, id3, id4) <- ids(4)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3, t4),
        capacity = 10,
        maxTasksPerBin = 2
      )
    } yield {
      expect(binQubits(out) == List(List(2, 2), List(2, 2)))
    }
  }

  test("assignToFinalBuckets with maxTasksPerBin = 1 puts every task in its own bin") {
    for {
      List(id1, id2, id3) <- ids(3)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(1),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3),
        capacity = 20,
        maxTasksPerBin = 1
      )
    } yield {
      expect(binQubits(out) == List(List(5), List(5), List(1)))
    }
  }

  test("assignToFinalBuckets uses first-fit among existing bins") {
    for {
      List(id1, id2, id3, id4) <- ids(4)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(4),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(4),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(4),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3, t4),
        capacity = 10,
        maxTasksPerBin = 10
      )
    } yield {
      expect(binQubits(out) == List(List(4, 4), List(4, 2)))
    }
  }

  test("assignToFinalBuckets keeps an oversized task in its own bin rather than rejecting it") {
    for {
      List(id1, id2, id3) <- ids(3)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(12),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(3),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(3),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
    
      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3),
        capacity = 10,
        maxTasksPerBin = 3
      )
    } yield {
      expect(binQubits(out) == List(List(12), List(3, 3)))
    }
  }

  test("assignToFinalBuckets with capacity 0 yields one bin per positive-size task") {
    for {
      List(id1, id2, id3) <- ids(3)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(3),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3),
        capacity = 0,
        maxTasksPerBin = 3
      )
    } yield {
      expect(binQubits(out) == List(List(3), List(2), List(2)))
    }
  }

  test("assignToFinalBuckets preserves deterministic bin order in the final output") {
    for {
      List(id1, id2, id3, id4, id5) <- ids(5)

      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(8),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t4 = QuantumTask(
        uuid = id4,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t5 = QuantumTask(
        uuid = id5,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(2),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      out = Scheduler.assignToFinalBuckets(
        bucket = List(t1, t2, t3, t4, t5),
        capacity = 10,
        maxTasksPerBin = 3
      )
    } yield {
      expect(binQubits(out) == List(List(8, 2), List(5, 5), List(2)))
    }
  }

   private def mkCandidate(
    device: Device,
    queueMillis: Long,
    runMillis: Long,
    fidelity: Double
  ): CandidateDevice =
    CandidateDevice(
      device = device,
      queueMillis = queueMillis,
      runMillis = runMillis,
      fidelity = fidelity
    )

  private def planView(plan: SynchronizedPlan): Map[String, List[Int]] =
    plan.assignments.map { case (device, tasks) =>
      device.platformId -> tasks.map(_.depth.value)
    }

  test("buildGreedySynchronizedPlan fails when a task has no candidates") { // possibly need to avoid this case all together
    for {
      List(id1) <- ids(1)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
        
      attempt <- Scheduler
        .buildGreedySynchronizedPlan[IO](
          orderedTasks = List(t1),
          candidatesByTask = Map.empty,
          t1BudgetMillis = 0L
        )
        .attempt
    } yield {
      expect(attempt.isLeft) and
      expect(attempt.swap.exists(_.getMessage == "No candidates for task"))
    }
  }

  test("for the first task, objective ties and the higher-fidelity candidate wins even with a worse queue") {
    for {
      List(id1) <- ids(1)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      b = Device(
        platform = "IBM",
        platformId = "B",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      candidates = Map(
        t1 -> List(
          mkCandidate(a, queueMillis = 0L,    runMillis = 1L, fidelity = 0.80),
          mkCandidate(b, queueMillis = 1000L, runMillis = 1L, fidelity = 0.95)
        )
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1),
        candidatesByTask = candidates,
        t1BudgetMillis = 0L
      )
    } yield {
      expect(planView(plan) == Map("B" -> List(10)))
    }
  }


  test("for the first task, if objective and fidelity tie, lower queueMillis wins") {
    for {
      List(id1) <- ids(1)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      b = Device(
        platform = "IBM",
        platformId = "B",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      candidates = Map(
        t1 -> List(
          mkCandidate(a, queueMillis = 0L,   runMillis = 1L, fidelity = 0.90),
          mkCandidate(b, queueMillis = 100L, runMillis = 1L, fidelity = 0.90)
        )
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1),
        candidatesByTask = candidates,
        t1BudgetMillis = 0L
      )
    } yield {
      expect(planView(plan) == Map("A" -> List(10)))
    }
  }

  test("for later tasks, lower objective beats higher fidelity") {
    for {
      List(id1, id2) <- ids(2)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      b = Device(
        platform = "IBM",
        platformId = "B",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      // t1 is forced onto A: queue 100, run 50 => A has finish 150.
      // For t2:
      //   A => start 150, finish 200
      //        current starts [100], finishes [150]
      //        spreadStart = 50, spreadFinish = 50, objective = 75
      //   B => start 100, finish 150
      //        spreadStart = 0, spreadFinish = 0, objective = 0
      // So B should win despite much worse fidelity.
      candidates = Map(
        t1 -> List(
          mkCandidate(a, queueMillis = 100L, runMillis = 50L, fidelity = 0.90)
        ),
        t2 -> List(
          mkCandidate(a, queueMillis = 100L, runMillis = 50L, fidelity = 0.99),
          mkCandidate(b, queueMillis = 100L, runMillis = 50L, fidelity = 0.10)
        )
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1, t2),
        candidatesByTask = candidates,
        t1BudgetMillis = 0L
      )
    } yield {
      expect(planView(plan) == Map(
        "A" -> List(10),
        "B" -> List(20)
      ))
    }
  }

  test("when objectives tie for a later task, higher fidelity wins") {
    for {
      List(id1, id2) <- ids(2)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      b = Device(
        platform = "IBM",
        platformId = "B",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      c = Device(
        platform = "IBM",
        platformId = "C",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      candidates = Map(
        t1 -> List(
          mkCandidate(a, queueMillis = 100L, runMillis = 50L, fidelity = 0.90)
        ),
        t2 -> List(
          mkCandidate(b, queueMillis = 100L, runMillis = 50L, fidelity = 0.80),
          mkCandidate(c, queueMillis = 100L, runMillis = 50L, fidelity = 0.95)
        )
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1, t2),
        candidatesByTask = candidates,
        t1BudgetMillis = 0L
      )
    } yield {
      expect(clue(planView(plan)) == Map(
        "A" -> List(10),
        "C" -> List(20)
      ))
    }
  }


   test("when objective and fidelity tie for a later task, lower queueMillis wins") {
    for {
      List(id1, id2) <- ids(2)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      b = Device(
        platform = "IBM",
        platformId = "B",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      c = Device(
        platform = "IBM",
        platformId = "C",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      // After t1 on A: current start 100, finish 150
      // Candidate B: start 90, finish 150 => spreadStart 10, spreadFinish 0 => objective 10
      // Candidate C: start 110, finish 150 => spreadStart 10, spreadFinish 0 => objective 10
      // Same fidelity, so lower queueMillis (90) should win.
      candidates = Map(
        t1 -> List(
          mkCandidate(a, queueMillis = 100L, runMillis = 50L, fidelity = 0.90)
        ),
        t2 -> List(
          mkCandidate(b, queueMillis = 90L,  runMillis = 60L, fidelity = 0.90),
          mkCandidate(c, queueMillis = 110L, runMillis = 40L, fidelity = 0.90)
        )
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1, t2),
        candidatesByTask = candidates,
        t1BudgetMillis = 0L
      )
    } yield {
      expect(planView(plan) == Map(
        "A" -> List(10),
        "B" -> List(20)
      ))
    }
  }

  test("with no T1 budget, the lower raw objective wins") {
    for {
      List(id1, id2) <- ids(2)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      b = Device(
        platform = "IBM",
        platformId = "B",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      c = Device(
        platform = "IBM",
        platformId = "C",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      // t1 forced onto A at start 0 finish 100
      // For t2:
      //   B => start 0, finish 120
      //        spreadStart 0, spreadFinish 20, objective 10
      //   C => start 30, finish 110
      //        spreadStart 30, spreadFinish 10, objective 35
      // With budget disabled (0L), B should win.
      candidates = Map(
        t1 -> List(
          mkCandidate(a, queueMillis = 0L, runMillis = 100L, fidelity = 0.90)
        ),
        t2 -> List(
          mkCandidate(b, queueMillis = 0L,  runMillis = 120L, fidelity = 0.90),
          mkCandidate(c, queueMillis = 30L, runMillis = 80L,  fidelity = 0.10)
        )
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1, t2),
        candidatesByTask = candidates,
        t1BudgetMillis = 0L
      )
    } yield {
      expect(planView(plan) == Map(
        "A" -> List(10),
        "B" -> List(20)
      ))
    }
  }

  test("T1 budget penalty can change the chosen device") {
    for {
      List(id1, id2) <- ids(2)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      b = Device(
        platform = "IBM",
        platformId = "B",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )
      c = Device(
        platform = "IBM",
        platformId = "C",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      // Same setup as previous test, but now budget = 10.
      //
      // B: spreadFinish = 20 > 10
      //    penalty = (20 - 10) * 10 = 100
      //    total objective = 10 + 100 = 110
      //
      // C: spreadFinish = 10 <= 10
      //    no penalty
      //    total objective = 35
      //
      // So C should win even though it has much worse fidelity.
      candidates = Map(
        t1 -> List(
          mkCandidate(a, queueMillis = 0L, runMillis = 100L, fidelity = 0.90)
        ),
        t2 -> List(
          mkCandidate(b, queueMillis = 0L,  runMillis = 120L, fidelity = 0.90),
          mkCandidate(c, queueMillis = 30L, runMillis = 80L,  fidelity = 0.10)
        )
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1, t2),
        candidatesByTask = candidates,
        t1BudgetMillis = 10L
      )
    } yield {
      expect(planView(plan) == Map(
        "A" -> List(10),
        "C" -> List(20)
      ))
    }
  }

  test("tasks assigned to the same device are kept in input order") {
    for {
      List(id1, id2, id3) <- ids(3)
      t1 = QuantumTask(
        uuid = id1,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(10),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t2 = QuantumTask(
        uuid = id2,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(20),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )
      t3 = QuantumTask(
        uuid = id3,
        circuit = Circuit(List.empty, 5),
        qubits = TaskQubits(5),
        shots = TaskShots(1000),
        depth = TaskDepth(30),
        parentTasks = List.empty,
        childTasks = List.empty,
        createdAt = LocalDateTime.now()
      )

      a = Device(
        platform = "IBM",
        platformId = "A",
        qubits = 20,
        t1 = 0f,
        t2 = 0f,
        gateSet = List.empty
      )

      candidates = Map(
        t1 -> List(mkCandidate(a, queueMillis = 0L, runMillis = 10L, fidelity = 0.90)),
        t2 -> List(mkCandidate(a, queueMillis = 0L, runMillis = 10L, fidelity = 0.90)),
        t3 -> List(mkCandidate(a, queueMillis = 0L, runMillis = 10L, fidelity = 0.90))
      )

      plan <- Scheduler.buildGreedySynchronizedPlan[IO](
        orderedTasks = List(t1, t2, t3),
        candidatesByTask = candidates,
        t1BudgetMillis = 0L
      )
    } yield {
      expect(planView(plan) == Map("A" -> List(10, 20, 30)))
    }
  }

  private final case class Calls(
    compileOrder: List[String] = Nil,
    fetchOrder: List[String] = Nil
  )

  private def mkClients(
    state: Ref[IO, FetchState],
    ibmByDeviceId: Map[String, IO[DeviceCalibration]]
  ): HttpClients[IO] = {
    val ibm = new IBMClient[IO] {
      def fetchBearerToken: IO[String] = ???
      def fetchDeviceInformation: IO[BackendsResponseV2] = ???
      def submitJob(r: SubmitJobRequestV2): IO[CreateJobResponseV2] = ??? 
      def listJobDetails(id: String): IO[JobDetailsResponseV2] = ???
      def getJobMetrics(id: String): IO[JobMetricsResponse] = ???
      def fetchDeviceCalibration(platformId: String): IO[DeviceCalibration] =
        state.update(s => s.copy(fetchOrder = s.fetchOrder :+ platformId)) *>
          ibmByDeviceId.getOrElse(
            platformId,
            IO.raiseError(new RuntimeException(s"unexpected calibration fetch for $platformId"))
          )
    }

    val azure = new AzureQuantumClient[IO] {
      def fetchDeviceInformation: IO[AzureDeviceStatusResponse] = ???
      def submitJob(jobId: String, jobRequest: AzureJobCreateRequest): IO[AzureJobResponse] = ???
      def getQuantumTask(jobId: String): IO[AzureJobResponse] = ???
      def fetchDeviceCalibration(platformId: String): IO[DeviceCalibration] =
        IO.raiseError(new RuntimeException(s"unexpected Azure calibration fetch for $platformId"))
    }

    val braket = new BraketClient[IO] {
      def fetchDeviceList: IO[BraketDeviceListResponse] = ???
      def fetchDeviceDetails(ids: List[String]): IO[List[BraketDeviceDetailsResponse]] = ???
      def submitBraketOpenQasmTask(r: BraketCreateQuantumTaskRequest, qasmSource:   String): IO[BraketCreateQuantumTaskResponse] = ??? 
      def getQuantumTask(taskId: String) : IO[BraketQuantumTaskResponse] = ???
      def fetchDeviceCalibration(platformId: String): IO[DeviceCalibration] =
        IO.raiseError(new RuntimeException(s"unexpected Braket calibration fetch for $platformId"))
    }

    HttpClients.fromParts[IO](ibm, braket, azure)
  }

  private val t0 = LocalDateTime.of(2026, 1, 1, 10, 0, 0)
  private val t1 = LocalDateTime.of(2026, 1, 1, 11, 0, 0)

  private def mkTask( //probably should use this in the previous tests 
    id: TaskId,
    circuit: Circuit,
    qubits: Int,
    shots: Int,
    depth: Int,
    parentTasks: List[TaskId],
    createdAt: LocalDateTime
  ): QuantumTask =
    QuantumTask(
      uuid = id,
      circuit = circuit,
      qubits = TaskQubits(qubits),
      shots = TaskShots(shots),
      depth = TaskDepth(depth),
      parentTasks = parentTasks,
      childTasks = List.empty,
      createdAt = createdAt
    )


  private def mkDevice(id: String, qubits: Int): Device =
    Device(
      platform = "IBM",
      platformId = id,
      qubits = qubits,
      t1 = 0f,
      t2 = 0f,
      gateSet = List.empty
    )
  
  private def mkIonQCalibration(avg1qFidelityPct: Double): IonQCalibration =
    IonQCalibration(
      avg1qFidelityPct = avg1qFidelityPct,
      avg2qFidelityPct = 100.0,
      avgReadoutFidelity = 1.0,
      t1Seconds = 1.0,
      t2Seconds = 1.0,
      oneQGateDurationSec = 1e-6,
      twoQGateDurationSec = 1e-6,
      readoutDurationSec = 1e-6
    )

   private val passthroughCompiler: FakeCompiler[IO] =
    FakeCompiler[IO](compiled = Nil)

  private final case class FetchState(fetchOrder: List[String] = Nil)
  
  test("flattenGroup returns the single task unchanged and does not call any client") {
    for {
      state <- Ref.of[IO, FetchState](FetchState())
      List(id1) <- ids(1)

      task = mkTask(
        id = id1,
        circuit = Circuit(List.empty, 5),
        qubits = 5,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = t0
      )

      clients = mkClients(state, Map.empty)

      out <- Scheduler.flattenGroup[IO](
        group = List(task),
        devices = List(mkDevice("A", 20)),
        clients = clients,
        compiler = passthroughCompiler,
        targetEstimatedFidelity = 0.0
      )

      seen <- state.get
    } yield {
      expect(out == List(task)) and
      expect(seen.fetchOrder.isEmpty)
    }
  }

  test("flattenGroup returns the original group unchanged when no device is feasible") {
    for {
      state <- Ref.of[IO, FetchState](FetchState())
      List(id1, id2, p1, p2) <- ids(4)

      tA = mkTask(
        id = id1,
        circuit = Circuit(List.empty, 6),
        qubits = 6,
        shots = 1000,
        depth = 10,
        parentTasks = List(p1),
        createdAt = t0
      )
      tB = mkTask(
        id = id2,
        circuit = Circuit(List.empty, 7),
        qubits = 7,
        shots = 2000,
        depth = 20,
        parentTasks = List(p2),
        createdAt = t1
      )

      clients = mkClients(state, Map.empty)

      out <- Scheduler.flattenGroup[IO](
        group = List(tA, tB),
        devices = List(
          mkDevice("small-1", 10),
          mkDevice("small-2", 12)
        ), // merged qubits = 13
        clients = clients,
        compiler = passthroughCompiler,
        targetEstimatedFidelity = 0.0
      )

      seen <- state.get
    } yield {
      expect(out == List(tA, tB)) and
      expect(seen.fetchOrder.isEmpty)
    }
  }

  test("flattenGroup only evaluates feasible devices") {
    for {
      state <- Ref.of[IO, FetchState](FetchState())
      List(id1, id2) <- ids(2)

      tA = mkTask(
        id = id1,
        circuit = Circuit(List(X(0)), 5),
        qubits = 5,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = t0
      )
      tB = mkTask(
        id = id2,
        circuit = Circuit(List.empty, 5),
        qubits = 5,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = t1
      )

      tooSmall = mkDevice("too-small", 8)
      feasibleA = mkDevice("A", 10)
      feasibleB = mkDevice("B", 15)

      clients = mkClients(
        state,
        Map(
          "A" -> IO.pure(mkIonQCalibration(avg1qFidelityPct = 100.0)),
          "B" -> IO.pure(mkIonQCalibration(avg1qFidelityPct = 100.0))
        )
      )

      _ <- Scheduler.flattenGroup[IO](
        group = List(tA, tB),
        devices = List(tooSmall, feasibleA, feasibleB),
        clients = clients,
        compiler = passthroughCompiler,
        targetEstimatedFidelity = -1.0
      )

      seen <- state.get
    } yield {
      expect(seen.fetchOrder == List("A", "B"))
    }
  }

  test("flattenGroup propagates calibration fetch failure and stops before later feasible devices") {
    for {
      state <- Ref.of[IO, FetchState](FetchState())
      List(id1, id2) <- ids(2)

      tA = mkTask(
        id = id1,
        circuit = Circuit(List(X(0)), 4),
        qubits = 4,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = t0
      )
      tB = mkTask(
        id = id2,
        circuit = Circuit(List.empty, 4),
        qubits = 4,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = t1
      )

      first = mkDevice("first", 10)
      second = mkDevice("second", 10)

      boom = new RuntimeException("fetch failed")

      clients = mkClients(
        state,
        Map(
          "first" -> IO.raiseError(boom),
          "second" -> IO.pure(mkIonQCalibration(avg1qFidelityPct = 100.0))
        )
      )

      attempt <- Scheduler.flattenGroup[IO](
        group = List(tA, tB),
        devices = List(first, second),
        clients = clients,
        compiler = passthroughCompiler,
        targetEstimatedFidelity = -1.0
      ).attempt

      seen <- state.get
    } yield {
      expect(attempt.swap.exists(_.getMessage == "fetch failed")) and
      expect(seen.fetchOrder == List("first"))
    }
  }

  private def mkClients2(
    state: Ref[IO, FetchState],
    ibmDevicesF: IO[BackendsResponseV2], //noticed I need this too late. Too lazy to patch up the tests now so using a new constructor
    ibmCalibrations: Map[String, IO[DeviceCalibration]]
  ): HttpClients[IO] = {

    val ibm = new IBMClient[IO] {
      def fetchDeviceInformation(): IO[BackendsResponseV2] =
        ibmDevicesF

      def fetchDeviceCalibration(platformId: String): IO[DeviceCalibration] =
        state.update(s => s.copy(fetchOrder = s.fetchOrder :+ platformId)) *>
          ibmCalibrations.getOrElse(
            platformId,
            IO.raiseError(new RuntimeException(s"unexpected calibration fetch for $platformId"))
          )

      def fetchBearerToken: IO[String] = ???
      def submitJob(r: SubmitJobRequestV2): IO[CreateJobResponseV2] = ??? 
      def listJobDetails(id: String): IO[JobDetailsResponseV2] = ???
      def getJobMetrics(id: String): IO[JobMetricsResponse] = ???
    }

    val braket = new BraketClient[IO] {
      def fetchDeviceDetails(ids: List[String]): IO[List[BraketDeviceDetailsResponse]] = 
        IO.pure(List.empty)
      def submitBraketOpenQasmTask(r: BraketCreateQuantumTaskRequest, qasmSource:   String): IO[BraketCreateQuantumTaskResponse] = ??? 
      def getQuantumTask(taskId: String) : IO[BraketQuantumTaskResponse] = ???
      def fetchDeviceList(): IO[BraketDeviceListResponse] =
        IO.pure(BraketDeviceListResponse(
          devices = List.empty,
          nextToken = None
        ))

      def fetchDeviceCalibration(platformId: String): IO[DeviceCalibration] =
        IO.raiseError(new RuntimeException(s"unexpected Braket calibration fetch for $platformId"))

    }

    val azure = new AzureQuantumClient[IO] {
      def fetchDeviceInformation: IO[AzureDeviceStatusResponse] = IO.pure(AzureDeviceStatusResponse(
        value = List.empty
      ))
      def submitJob(jobId: String, jobRequest: AzureJobCreateRequest): IO[AzureJobResponse] = ???
      def getQuantumTask(jobId: String): IO[AzureJobResponse] = ???
      def fetchDeviceCalibration(platformId: String): IO[DeviceCalibration] =
        IO.raiseError(new RuntimeException(s"unexpected Azure calibration fetch for $platformId"))
    }

    HttpClients.fromParts[IO](ibm, braket, azure)
  }


  private def mkIBMDevice(id: String, qubits: Int): IBMBackendDevice =
    IBMBackendDevice(
      name = id,
      status = IBMBackendDeviceStatus(name = "online", reason = None),
      qubits = Some(qubits),
      queue_length = 0,
      is_simulator = None, 
      clops = None,
      processor_type = None,
      performance_metrics = None,
      wait_time_seconds = None,
    )

  test("attemptToMergeSyncTasks returns a single task unchanged") {
    for {
      state <- Ref.of[IO, FetchState](FetchState())
      List(id1) <- ids(1)

      t1 = mkTask(
        id = id1,
        circuit = Circuit(List.empty, 5),
        qubits = 5,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = t0
      )

      clients = mkClients2(
        state = state,
        ibmDevicesF = IO.pure(BackendsResponseV2(List(mkIBMDevice("big", 20)))),
        ibmCalibrations = Map("big" -> IO.pure(mkIonQCalibration(100.0)))
      )

      out <- Scheduler.attemptToMergeSyncTasks[IO](
        tasks = List(t1),
        clients = clients,
        compiler = passthroughCompiler,
        targetEstimatedFidelity = -1e9
      )

      seen <- state.get
    } yield {
      expect(out == List(t1)) and
      expect(seen.fetchOrder.isEmpty)
    }
  }

  private val ts0 = LocalDateTime.of(2026, 1, 1, 10, 0, 0)
  private val ts1 = LocalDateTime.of(2026, 1, 1, 11, 0, 0)
  test("attemptToMergeSyncTasks returns tasks unchanged when no devices are available") {
    for {
      state <- Ref.of[IO, FetchState](FetchState())
      List(id1, id2) <- ids(2)

      t1 = mkTask(
        id = id1,
        circuit = Circuit(List.empty, 3),
        qubits = 3,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = ts0
      )
      t2 = mkTask(
        id = id2,
        circuit = Circuit(List.empty, 4),
        qubits = 4,
        shots = 1000,
        depth = 30,
        parentTasks = Nil,
        createdAt = ts1
      )

      clients = mkClients2(
        state = state,
        ibmDevicesF = IO.pure(BackendsResponseV2(List.empty)),
        ibmCalibrations = Map.empty
      )

      out <- Scheduler.attemptToMergeSyncTasks[IO](
        tasks = List(t1, t2),
        clients = clients,
        compiler = passthroughCompiler,
        targetEstimatedFidelity = -1e9
      )

      seen <- state.get
    } yield {
      expect(out == List(t1, t2)) and
      expect(seen.fetchOrder.isEmpty)
    }
  }

  test("attemptToMergeSyncTasks propagates calibration fetch failure from the first merged group") {
    for {
      state <- Ref.of[IO, FetchState](FetchState())
      List(id1, id2) <- ids(2)

      t1 = mkTask(
        id = id1,
        circuit = Circuit(List.empty, 4),
        qubits = 4,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = ts0
      )
      t2 = mkTask(
        id = id2,
        circuit = Circuit(List.empty, 4),
        qubits = 4,
        shots = 1000,
        depth = 10,
        parentTasks = Nil,
        createdAt = ts1
      )

      boom = new RuntimeException("fetch failed")

      clients = mkClients2(
        state = state,
        ibmDevicesF = IO.pure(BackendsResponseV2(List(mkIBMDevice("big", 10)))),
        ibmCalibrations = Map(
          "big" -> IO.raiseError(boom)
        )
      )

      attempt <- Scheduler.attemptToMergeSyncTasks[IO](
        tasks = List(t1, t2),
        clients = clients,
        compiler = passthroughCompiler,
        targetEstimatedFidelity = -1e9
      ).attempt

      seen <- state.get
    } yield {
      expect(attempt.swap.exists(_.getMessage == "fetch failed")) and
      expect(seen.fetchOrder == List("big"))
    }
  }
}
