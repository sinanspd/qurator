package qurator.testbed

import cats._
import cats.effect._
import cats.syntax.all._
import org.typelevel.log4cats.Logger

import java.time.{Duration, LocalDateTime, Instant}

import qurator.domain.Task._
import qurator.domain.device._
import qurator.programs.DeviceEstimator
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.effects.GenUUID
import qurator.domain.ID
import qurator.testbed.FakeCompiler
import qurator.modules.HttpClients
import qurator.programs.Scheduler
import qurator.domain.Braket._
import qurator.domain.IBM._
import qurator.clients.AzureQuantumClient
import qurator.domain.Azure._
import qurator.clients.BraketClient
import qurator.clients.IBMClient
import qurator.testbed.IBMCalibrationInstances._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import qurator.util.QuantumTaskLoader
import qurator.util.Qasm3Parser
import fs2.io.file.Path

final case class QuantumTaskSpec(
    circuit: Circuit,
    qubits: TaskQubits,
    shots: TaskShots,
    depth: TaskDepth
)

object WorkloadSpecs { 
    //For test only, remove later 
    val defaultT: Vector[QuantumTaskSpec] =
        Vector(
        QuantumTaskSpec(Circuit(List(X(0), Measure(0)), 1), TaskQubits(1), TaskShots(1000), TaskDepth(1)),
        QuantumTaskSpec(Circuit(List(H(0), Measure(0)), 1), TaskQubits(1), TaskShots(1000), TaskDepth(1)),
        QuantumTaskSpec(Circuit(List(X(0), H(0), Measure(0)), 1), TaskQubits(1), TaskShots(2000), TaskDepth(2)),
        QuantumTaskSpec(Circuit(List(CX(0, 1), Measure(1)), 2), TaskQubits(2), TaskShots(1500), TaskDepth(1)),
        QuantumTaskSpec(Circuit(List(H(0), CX(0, 1), Measure(0)), 2), TaskQubits(2), TaskShots(1500), TaskDepth(2)),
        QuantumTaskSpec(Circuit(List(X(0), X(1), CZ(0, 1), Measure(0)), 2), TaskQubits(2), TaskShots(3000), TaskDepth(3)),
        QuantumTaskSpec(Circuit(List(X(0), H(1), Swap(0, 1), Measure(0)), 2), TaskQubits(2), TaskShots(2500), TaskDepth(3))
        )

    val loadedTasks: IO[Vector[QuantumTaskSpec]] = 
        QuantumTaskLoader.load(
            QuantumTaskLoader.Settings(
                folder = Path("mqt"),
                shots = 1000,
                parseConfig = Qasm3Parser.ParseConfig.lenientSkipUnsupported
            )
        )

    def sample(n: Int, seed: Long, T: Vector[QuantumTaskSpec]): IO[List[QuantumTaskSpec]] = 
        if(n <= 0 || T.isEmpty) List.empty[QuantumTaskSpec].pure[IO]
        else Sync[IO].delay(new scala.util.Random(seed)).map { rng => 
            List.fill(n)(T(rng.nextInt(T.size)))
        }
}

final case class JobRecord(
  taskId: TaskId,
  deviceId: String,
  submittedAt: LocalDateTime,
  startedAt: LocalDateTime,
  finishedAt: LocalDateTime,
  queueWaitMillis: Long,
  runMillis: Long
) 

//This will replace FakeDevice in the testbed. 
final class BenchmarkFakeDevice private (
  val device: Device,
  //deviceEstimator: DeviceEstimator[IO],
  queueLen: Int,
  msPerGate: Long,
  jobsRef: Ref[IO, Vector[JobRecord]]
) {

  private def nowF: IO[LocalDateTime] =
    Sync[IO].delay(LocalDateTime.now())

//   private def processingMillisForDevice: IO[Long] =
//     deviceEstimator.estimateDeviceProcessingSpeed(device.platformId).map {
//       case (min0, max0) =>
//         val lo = math.min(min0, max0).toLong
//         val hi = math.max(min0, max0).toLong
//         if (hi <= lo) lo else (lo + hi) / 2L
//     }

  private def localBacklogMillis(now: LocalDateTime, existing: Vector[JobRecord]): Long =
    existing.lastOption match {
      case None => 0L
      case Some(last) =>
        if (last.finishedAt.isAfter(now)) Duration.between(now, last.finishedAt).toMillis
        else 0L
    }

//   def submitJob(taskId: TaskId): IO[JobRecord] =
//     for {
//       now <- nowF
//       existing <- jobsRef.get
//       //queueLen <- deviceEstimator.estimateDeviceQueueLength(device.platformId)
//       //procMillis <- processingMillisForDevice

//       externalQueueMillis = queueLen.toLong // * procMillis
//       localQueueMillis = localBacklogMillis(now, existing)
//       totalQueueMillis = externalQueueMillis + localQueueMillis

//       startAt = now.plusNanos(totalQueueMillis * 1000000L)
//       finishAt = startAt.plusNanos(procMillis * 1000000L)

//       rec = JobRecord(
//         taskId = taskId,
//         deviceId = device.platformId,
//         submittedAt = now,
//         startedAt = startAt,
//         finishedAt = finishAt,
//         queueWaitMillis = totalQueueMillis,
//         runMillis = procMillis
//       )

//       _ <- jobsRef.update(_ :+ rec)
//     } yield rec

def submitJob(taskId: TaskId): IO[JobRecord] =
  for {
    now <- nowF
    existing <- jobsRef.get

    val jobMillis: Long = msPerGate * 100L
    val externalQueueMillis: Long = queueLen.toLong * jobMillis
    val localQueueMillis: Long = localBacklogMillis(now, existing)
    val totalQueueMillis: Long = externalQueueMillis + localQueueMillis

    val startAt: LocalDateTime  = now.plusNanos(totalQueueMillis * 1000000L)
    val finishAt: LocalDateTime = startAt.plusNanos(jobMillis * 1000000L)

    val rec = JobRecord(
      taskId = taskId,
      deviceId = device.platformId,
      submittedAt = now,
      startedAt = startAt,
      finishedAt = finishAt,
      queueWaitMillis = totalQueueMillis,
      runMillis = jobMillis
    )

    _ <- jobsRef.update(_ :+ rec)
  } yield rec

  def estimatedCurrentQueueWaitMillis: IO[Long] =
    for {
        now <- nowF
        existing <- jobsRef.get

        val jobMillis: Long = msPerGate * 100L
        val externalQueueMillis: Long = queueLen.toLong * jobMillis
        val localQueueMillis: Long = localBacklogMillis(now, existing)
    } yield externalQueueMillis + localQueueMillis


  def jobRecord(taskId: TaskId): IO[Option[JobRecord]] =
    jobsRef.get.map(_.find(_.taskId == taskId))

//   def estimatedCurrentQueueWaitMillis: IO[Long] =
//     for {
//       now <- nowF
//       existing <- jobsRef.get
//       queueLen <- deviceEstimator.estimateDeviceQueueLength(device.platformId)
//       procMillis <- processingMillisForDevice
//       externalQueueMillis = queueLen.toLong * procMillis
//       localQueueMillis = localBacklogMillis(now, existing)
//     } yield externalQueueMillis + localQueueMillis
}

object BenchmarkFakeDevice {
//   def make(
//     device: Device,
//     deviceEstimator: DeviceEstimator[IO]
//   ): IO[BenchmarkFakeDevice] =
//     Ref
//       .of[IO, Vector[JobRecord]](Vector.empty)
//       .map(ref => new BenchmarkFakeDevice(device, deviceEstimator, ref))
// }

    def make(device: Device, queueLen: Int, msPerGate: Long): IO[BenchmarkFakeDevice] = 
        Ref
        .of[IO, Vector[JobRecord]](Vector.empty)
        .map(ref => new BenchmarkFakeDevice(device, queueLen, msPerGate, ref))
}

final case class BenchmarkDeviceRegistry(
  devicesById: Map[String, Device],
  fakeDevicesById: Map[String, BenchmarkFakeDevice],
  calibrationsById: Map[String, DeviceCalibration],
  queueLenByDeviceId: Map[String, Int],   
  msPerGate: Long = 5L
) {

  def queueLen(deviceId: String): Int =
    queueLenByDeviceId.getOrElse(deviceId, 0)

  def devices: List[Device] = devicesById.values.toList

  def device(deviceId: String): Device =
    devicesById(deviceId)

  def fakeDevice(deviceId: String): BenchmarkFakeDevice =
    fakeDevicesById(deviceId)

  def calibration(deviceId: String): DeviceCalibration =
    calibrationsById(deviceId)
}

object BenchmarkDeviceRegistry {

    //just for initial tests, will remove later 
    def defaultDevices: List[Device] =
        List(
            Device(
                platform = "Braket",
                platformId = "braket-rigetti-ankaa",
                qubits = 82,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ),
            // Device(
            //     platform = "Braket",
            //     platformId = "braket-iqm-garnet",
            //     qubits = 20,
            //     t1 = 0f,
            //     t2 = 0f,
            //     gateSet = List.empty
            // ),
            Device(
                platform = "Braket",
                platformId = "braket-aqt-ibex-q1" ,
                qubits = 12,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ),
            Device(
                platform = "Braket",
                platformId = "braket-ionq-forte-1",
                qubits = 36,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ),
            // Device(
            //     platform = "Braket",
            //     platformId = "braket-quera-aquila",
            //     qubits = 256,
            //     t1 = 0f,
            //     t2 = 0f,
            //     gateSet = List.empty
            // ), 
            Device(
                platform = "IBM",
                platformId = "ibm_boston",
                qubits = 156,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ),
            Device(
                platform = "IBM",
                platformId = "ibm_kingston",
                qubits = 156,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ), 
            Device(
                platform = "IBM",
                platformId = "ibm_pittsburg",
                qubits = 156,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ), 
            Device(
                platform = "IBM",
                platformId = "ibm_fez",
                qubits = 156,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ),
            Device(
                platform = "IBM",
                platformId = "ibm_marrakesh",
                qubits = 156,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ), 
            Device(
                platform = "IBM",
                platformId = "ibm_torino",
                qubits = 133,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            )
        )
    ///saaaaaymmmm 
    def defaultCalibrations: Map[String, DeviceCalibration] =
        Map(
            "braket-rigetti-ankaa" -> //
                RigettiCalibration( 
                    avg1qFidelityPct = 97.946, //
                    readoutFidelityPct = 95.472, // 
                    swapFidelityPct = 90.130, // 
                    t1Seconds = 3.6387e-5, //8
                    t2Seconds = 2.2118e-5, // 
                    swapGateDurationNs = 300,
                    readoutDurationNs = 1200,
                    oneQGateDurationNs = 40,
                    twoQGateDurationNs = 140
                ),
            "braket-iqm-garnet" -> //
                IQMCalibration(
                    t1 = 3.3829e-5,
                    t2 = 8.925e-6, 
                    q1fidelity = 99.296,
                    q2fidelity = 99.884, 
                    readoutFidelity = 97.940
                    // typicalDetectionFalsePositive = 0.02,
                    // typicalDetectionFalseNegative = 0.03,
                    // typicalVacancyError = Some(0.04),
                    // typicalFillingError = None,
                    // typicalAtomLossProbability = Some(0.03),
                    // t1SingleSec = Some(7.0),
                    // t2EchoSingleSec = Some(4.0),
                    // t2SingleSec = Some(3.5)
                ),
            "braket-aqt-ibex-q1" -> //
                AQTCalibration(
                    t1Seconds = 1.168, 
                    t2Seconds = 0.1632,
                    readoutFidelity = 99.740, 
                    readoutDurationSec = 0.0015,
                    oneQGateDurationSec = 3e-5,
                    oneQGateFidelity = 99.978, 
                    twoQGateDurationSec = 0.000335,
                    twoQGateFidelity = 98.500
                ),
            "braket-ionq-forte-1" ->  // 
                IonQCalibration(
                    t1Seconds = 100,
                    t2Seconds = 1,
                    avg1qFidelityPct = 99.93177087236276,
                    avg2qFidelityPct = 97.41735442219582, 
                    avgReadoutFidelity = 99.15,
                    oneQGateDurationSec = 130e-6,
                    twoQGateDurationSec = 970e-6,
                    readoutDurationSec = 150e-6
                ),
            "braket-quera-aquila" -> //
                QuEraCalibration(
                    typicalDetectionFalsePositive = 0.001,
                    typicalDetectionFalseNegative = 0.001,
                    typicalVacancyError = Some(0.001),     
                    typicalFillingError = Some(0.008),       
                    typicalAtomLossProbability = Some(0.001),
                    t1SingleSec = Some(7.5e-5),
                    t2EchoSingleSec = Some(8e-6),
                    t2SingleSec = Some(5e-6)
                ),
            "ibm_boston" -> ibmBostonCalibration,
            "ibm_kingston" -> ibmKingstonCalibration,
            "ibm_pittsburg" -> ibmPittsburghCalibration, 
            "ibm_fez" -> ibmFezCalibration, 
            "ibm_marrakesh" -> ibmMarrakeshCalibration, 
            "ibm_torino" -> ibmTorinoCalibration
        )

    implicit val logger = Slf4jLogger.getLogger[IO]

    def make(
        devices: List[Device],
        calibrationsById: Map[String, DeviceCalibration],
        deviceEstimator: DeviceEstimator[IO],
        msPerGate: Long = 5L
    ): IO[BenchmarkDeviceRegistry] =
        for{
          _ <- Logger[IO].info("Created Benchmark Device Registry")  
          byId = devices.map(d => d.platformId -> d).toMap
          qMap: Map[String, Int] =
            byId.keys.map { id =>
                val q =  scala.util.Random.between(0, 5) //(math.abs(id.hashCode) % 50) + 1 //needs to be 0 for sync baselines 
                id -> q
            }.toMap
            fakePairs <- devices.traverse { d =>
                val qLen = qMap(d.platformId)
                BenchmarkFakeDevice.make(d, qLen, msPerGate).map(fd => d.platformId -> fd)
            }
        } yield BenchmarkDeviceRegistry(
                    devicesById = byId,
                    calibrationsById = calibrationsById,
                    queueLenByDeviceId = qMap,
                    fakeDevicesById = fakePairs.toMap,
                    msPerGate = msPerGate
                )
    
        

  
        // devices
        // .traverse { d =>
        //     BenchmarkFakeDevice.make(d, deviceEstimator).map(fd => d.platformId -> fd)
        // }
        // .map { fakePairs =>
        //     BenchmarkDeviceRegistry(
        //         devicesById = devices.map(d => d.platformId -> d).toMap,
        //         fakeDevicesById = fakePairs.toMap,
        //         calibrationsById = calibrationsById
        //     )
        // }
}

final case class SubmittedQuantum(taskId: TaskId, deviceId: String)

final case class QuantumTaskMetric(
    taskId: TaskId,
    deviceId: String,
    queueWaitMillis: Long,
    predictedLogFidelity: Double,
    predictedSuccessProbability: Double
)

final case class BenchmarkRun(
    policyName: String,
    selectedQuantumTasks: Int,
    schedulingWallMillis: Long,
    quantumMetrics: List[QuantumTaskMetric]
) {
    lazy val throughputQuantumPerSec: Double =
        if (schedulingWallMillis <= 0L) selectedQuantumTasks.toDouble
        else selectedQuantumTasks.toDouble / (schedulingWallMillis.toDouble / 1000.0)

    lazy val meanQueueWaitMillis: Double =
        if (quantumMetrics.isEmpty) 0.0
        else quantumMetrics.map(_.queueWaitMillis.toDouble).sum / quantumMetrics.size.toDouble

    lazy val meanPredictedLogFidelity: Double =
        if (quantumMetrics.isEmpty) 0.0
        else quantumMetrics.map(_.predictedLogFidelity).sum / quantumMetrics.size.toDouble

    lazy val geometricMeanPredictedSuccessProbability: Double =
                math.exp(meanPredictedLogFidelity)

    lazy val meanPredictedSuccessProbability: Double =
        if (quantumMetrics.isEmpty) 0.0
        else quantumMetrics.map(_.predictedSuccessProbability).sum / quantumMetrics.size.toDouble
}


final case class FakeBenchmarkClients(
    braketDeviceList: IO[BraketDeviceListResponse],
    braketDeviceDetails: List[String] => IO[List[BraketDeviceDetailsResponse]],
    braketSubmit: (BraketCreateQuantumTaskRequest, String) => IO[BraketCreateQuantumTaskResponse],
    braketGetJob: String => IO[BraketQuantumTaskResponse],

    ibmFetchBearerToken: IO[String],
    ibmDeviceInfo: IO[BackendsResponseV2],
    ibmSubmit: SubmitJobRequestV2 => IO[CreateJobResponseV2],
    ibmListJob: String => IO[JobDetailsResponseV2],
    ibmMetrics: String => IO[JobMetricsResponse]
)


object DummyResponses {

  private def nowIso: String = Instant.now().toString

  def braketDevice(deviceArn: String, name: String, provider: String, status: String = "ONLINE"): BraketDevice =
    BraketDevice(
      deviceArn = deviceArn,
      deviceName = name,
      deviceCapabilities = "{}",
      deviceStatus = status,
      deviceType = "QPU",
      providerName = provider
    )

  def braketDeviceListResponse(devices: List[BraketDevice]): BraketDeviceListResponse =
    BraketDeviceListResponse(
      devices = devices,
      nextToken = None
    )

  def braketQueueInfo(queue: String = "QUANTUM_TASKS_QUEUE", size: String = "0"): BraketDeviceQueueInfo =
    BraketDeviceQueueInfo(
      queue = queue,
      queuePriority = None,
      queueSize = size
    )

  def braketDeviceDetailsResponse(
    deviceArn: String,
    name: String,
    provider: String,
    queueSize: String = "0",
    status: String = "ONLINE"
  ): BraketDeviceDetailsResponse =
    BraketDeviceDetailsResponse(
      deviceArn = deviceArn,
      deviceName = name,
      deviceStatus = status,
      deviceType = "QPU",
      providerName = provider,
      deviceCapabilities = "{}",
      deviceQueueInfo = List(braketQueueInfo(size = queueSize))
    )

  def braketCreateQuantumTaskResponse(taskArn: String): BraketCreateQuantumTaskResponse =
  BraketCreateQuantumTaskResponse(quantumTaskArn = taskArn)

  def braketQuantumTaskResponse(
    taskArn: String,
    deviceArn: String,
    status: String = "COMPLETED",
    shots: Int = 1000
  ): BraketQuantumTaskResponse =
    BraketQuantumTaskResponse(
      actionMetadata = BraketActionMetadata(
        actionType = "OPENQASM",
        executableCount = 1,
        programCount = 1
      ),
      associations = Nil,
      createdAt = nowIso,
      deviceArn = deviceArn,
      deviceParameters = "{}",
      endedAt = Some(nowIso),
      experimentalCapabilities = None,
      failureReason = None,
      numSuccessfulShots = shots,
      outputS3Bucket = None,
      outputS3Directory = None,
      quantumTaskArn = taskArn,
      queueInfo = braketQueueInfo(size = "0"),
      shots = shots,
      status = status,
      tags = None
    )

  def ibmBackendDevice(
    name: String,
    qubits: Int,
    queueLength: Int = 0,
    statusName: String = "active"
  ): IBMBackendDevice =
    IBMBackendDevice(
      name = name,
      status = IBMBackendDeviceStatus(name = statusName, reason = None),
      is_simulator = Some(false),
      qubits = Some(qubits),
      clops = None,
      processor_type = None,
      queue_length = queueLength,
      performance_metrics = None,
      wait_time_seconds = Some(IBMBackendDeviceWaitTimeSeconds(average = 0, p50 = 0, p95 = 0))
    )

  def ibmBackendsResponseV2(devices: List[IBMBackendDevice]): BackendsResponseV2 =
    BackendsResponseV2(devices = devices)

  def ibmCreateJobResponseV2(id: String, backend: String): CreateJobResponseV2 =
    CreateJobResponseV2(
      id = id,
      backend = backend,
      session_id = None,
      `private` = None,
      calibration_id = None
    )

  def ibmJobDetailsResponseV2(
    id: String,
    backend: String,
    status: String = "Completed"
  ): JobDetailsResponseV2 =
    JobDetailsResponseV2(
      id = id,
      backend = backend,
      state = JobState(
        status = status,
        reason = None,
        reason_code = None,
        reason_solution = None
      ),
      status = status,
      created = nowIso,
      program = JobProgram(id = "dummy-program"),
      runtime = None,
      cost = 0,
      tags = None,
      session_id = None,
      user_id = "bench-user",
      `private` = None,
      estimated_running_time_seconds = None,
      calibration_id = None
    )

  def ibmJobMetricsResponse(
    positionInQueue: Int = 0
  ): JobMetricsResponse =
    JobMetricsResponse(
      timestamps = JobTimeStamps(
        created = nowIso,
        finished = Some(nowIso),
        running = Some(nowIso)
      ),
      bss = JobBSS(seconds = 0),
      usage = JobUsage(quantum_seconds = 0, seconds = 0),
      qiskit_version = "bench",
      estimated_start_time = None,
      estimated_completion_time = None,
      position_in_queue = Some(positionInQueue),
      position_in_provider = None
    )

  def benchmarkClientDummies[F[_]: MonadThrow](
    braketDevices: List[BraketDevice],
    braketDetails: List[BraketDeviceDetailsResponse],
    ibmDevices: List[IBMBackendDevice],
    defaultBraketArn: String,
    defaultIbmBackend: String
  ): FakeBenchmarkClients = {

    val braketList = braketDeviceListResponse(braketDevices)
    val braketDetailsMap = braketDetails.map(d => d.deviceArn -> d).toMap

    FakeBenchmarkClients(
      braketDeviceList = braketList.pure[IO],
      braketDeviceDetails = ids =>
        ids.traverse { arn =>
          braketDetailsMap.get(arn) match {
            case Some(d) => d.pure[IO]
            case None    => braketDeviceDetailsResponse(arn, name = "unknown", provider = "unknown").pure[IO]
          }
        },
      braketSubmit = (req, _) => {
            val taskArn = s"arn:aws:braket:bench:task/${req.clientToken}"
            braketCreateQuantumTaskResponse(taskArn).pure[IO]
        },
      braketGetJob = taskId => {
        val taskArn = if (taskId.startsWith("arn:")) taskId else s"arn:aws:braket:bench:task/$taskId"
        braketQuantumTaskResponse(taskArn = taskArn, deviceArn = defaultBraketArn).pure[IO]
      },

      ibmFetchBearerToken = "dummy-token".pure[IO],
      ibmDeviceInfo = ibmBackendsResponseV2(ibmDevices).pure[IO],
      ibmSubmit = _ => ibmCreateJobResponseV2(id = "bench-job-1", backend = defaultIbmBackend).pure[IO],
      ibmListJob = id => ibmJobDetailsResponseV2(id = id, backend = defaultIbmBackend, status = "Completed").pure[IO],
      ibmMetrics = _ => ibmJobMetricsResponse(positionInQueue = 0).pure[IO]
    )
  }
}


object BenchmarkHttpClients{

    def make(
        registry: BenchmarkDeviceRegistry,
        dummies: FakeBenchmarkClients
    ): HttpClients[IO] = {
        val azure = new AzureQuantumClient[IO]{
            def fetchDeviceInformation: IO[AzureDeviceStatusResponse] = 
                AzureDeviceStatusResponse(value = List()).pure[IO]
            def submitJob(jobId: String, jobRequest: AzureJobCreateRequest): IO[AzureJobResponse] = ??? 
            def getQuantumTask(jobId: String): IO[AzureJobResponse] = ??? 
            def fetchDeviceCalibration(deviceId: String): IO[DeviceCalibration] = ???
        }


        val braket = new BraketClient[IO]{
            def fetchDeviceList: IO[BraketDeviceListResponse] = dummies.braketDeviceList
            def fetchDeviceDetails(ids: List[String]): IO[List[BraketDeviceDetailsResponse]] = dummies.braketDeviceDetails(ids)
            def submitBraketOpenQasmTask(r: BraketCreateQuantumTaskRequest, qasmSource: String): IO[BraketCreateQuantumTaskResponse] =
                dummies.braketSubmit(r, qasmSource)
            def getQuantumTask(taskId: String) : IO[BraketQuantumTaskResponse] = dummies.braketGetJob(taskId)
            def fetchDeviceCalibration(deviceArn: String): IO[DeviceCalibration] = registry.calibration(deviceArn).pure[IO]
        }

        val ibm = new IBMClient[IO]{
            def fetchBearerToken: IO[String] = dummies.ibmFetchBearerToken
            def fetchDeviceInformation: IO[BackendsResponseV2] = dummies.ibmDeviceInfo
            def submitJob(r: SubmitJobRequestV2): IO[CreateJobResponseV2] =  dummies.ibmSubmit(r) 
            def listJobDetails(id: String): IO[JobDetailsResponseV2] = dummies.ibmListJob(id)
            def getJobMetrics(id: String): IO[JobMetricsResponse] = dummies.ibmMetrics(id)
            def fetchDeviceCalibration(deviceArn: String): IO[DeviceCalibration] = registry.calibration(deviceArn).pure[IO]
        }
        HttpClients.fromParts[IO](ibm, braket, azure)
    }
}

object SchedulerBenchmarkRunner {
    implicit val logger = Slf4jLogger.getLogger[IO]
    sealed trait BaselinePolicy {
        def name: String
    }

    object BaselinePolicy {
        case object LeastBusy extends BaselinePolicy {
            val name = "least_busy"
        }
        case object HighestFidelity extends BaselinePolicy {
            val name = "highest_fidelity"
        }
    }

    private def monotonicMillis: IO[Long] =
        Temporal[IO].monotonic.map(_.toMillis)
    

    private def submitOneWorkItem(
        scheduler: Scheduler[IO], 
        spec: QuantumTaskSpec
    ): IO[List[(TaskId, QuantumTaskSpec)]] =
        for{ 
            npw <- LocalDateTime.now().pure[IO]
            paretReq = new NewClassicalTaskRequest(
                program = (),
                parentTasks = Nil, //TODO: This obv shouldn't be the case for the full suite, we need longer ancestoral chains 
                childTasks = Nil,
                createdAt = npw
            )
            _ <- Logger[IO].info("Submitted one task to the scheduler")
            parentId <- scheduler.submitTask(paretReq)
            quantumReq = NewQuantumTaskRequest(
                circuit = spec.circuit,
                qubits = spec.qubits,
                shots = spec.shots,
                depth = spec.depth,
                parentTasks = parentId, 
                childTasks = Nil,
                createdAt = npw
            )
            quantumIds <- scheduler.submitTask(quantumReq)
            childReq = NewClassicalTaskRequest(
                program = (),
                parentTasks = quantumIds,
                childTasks = Nil,
                createdAt = npw
            )
            _ <- scheduler.submitTask(childReq)
        } yield quantumIds.map(id => (id, spec))


    private def waitUntilAllSubmitted(
        scheduler: Scheduler[IO],
        registry: BenchmarkDeviceRegistry,
        expectedQuantumIds: Set[TaskId],
        pollEvery: scala.concurrent.duration.FiniteDuration
    ): IO[List[SubmittedQuantum]] = {
        def loop(seen: Map[TaskId, SubmittedQuantum]): IO[List[SubmittedQuantum]] =
            scheduler.getSubmittedTasks().flatMap { raw => 
                val newOnes = 
                    raw.flatMap { case (s1, s2, jid, tid) => 
                        if(!expectedQuantumIds.contains(tid)) None  
                        else{
                            Some(SubmittedQuantum(tid, s2))
                        }
                    }    
                val mergedSeen = newOnes.foldLeft(seen)((acc, sq) => acc.updated(sq.taskId, sq))
                if(expectedQuantumIds.subsetOf(mergedSeen.keySet)) mergedSeen.values.toList.pure[IO]
                else Temporal[IO].sleep(pollEvery) *> loop(mergedSeen)
            }
            
        if(expectedQuantumIds.isEmpty) List.empty[SubmittedQuantum].pure[IO]
        else loop(Map.empty)
    }

    private def quantumMetricsForAssignments(
        assignments: List[SubmittedQuantum],
        quantumById: Map[TaskId, QuantumTaskSpec],
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO]
    ): IO[List[QuantumTaskMetric]] =
        assignments.traverse{ sub =>
            val spec = quantumById(sub.taskId)
            val device = registry.device(sub.deviceId)

            for{
                rec <- registry.fakeDevice(sub.deviceId).submitJob(sub.taskId)
                est <- Scheduler.estimateFidelity[IO](device, spec.circuit, clients, compiler)
            } yield QuantumTaskMetric(
                taskId = sub.taskId,
                deviceId = sub.deviceId,
                queueWaitMillis = rec.queueWaitMillis,
                predictedLogFidelity = est.logPTotal,
                predictedSuccessProbability = est.pTotal
            )
        }



    def runSchedulerBenchmark(
        scheduler: Scheduler[IO], 
        specs: List[QuantumTaskSpec],
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO],
        pollEvery: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.DurationInt(100).millis
    ): IO[BenchmarkRun] = {
        for{
            t0 <- monotonicMillis
            report <- WorkloadSpecs.loadedTasks
            _ <- Logger[IO].info(s"Loaded ${report.size} task(s)")
            _ <- Logger[IO].info("Starting Scheduler Benchmark")
            quantumIdPairs <- specs.traverse(submitOneWorkItem(scheduler, _)).map(_.flatten)
            _ <- Logger[IO].info(s"QuantumIds: ${quantumIdPairs.mkString(", ")}")
            specById = quantumIdPairs.toMap
            expectedIds = specById.keySet
            submitted <- waitUntilAllSubmitted(scheduler, registry, expectedIds, pollEvery)
            t1 <- monotonicMillis
            metrics <- quantumMetricsForAssignments(submitted, specById, registry, clients, compiler)

        } yield BenchmarkRun(
            policyName = "scheduler",
            selectedQuantumTasks = expectedIds.size,
            schedulingWallMillis = t1 - t0,
            quantumMetrics = metrics
        )
    }


    private def chooseLeastBusyDevice(
        task: QuantumTaskSpec,
        registry: BenchmarkDeviceRegistry
    ): IO[Device] = {
        val feasible = registry.devices.filter(_.qubits >= task.qubits.value)
        if (feasible.isEmpty) {
            new RuntimeException(s"No feasible device for task").raiseError[IO, Device]
        } else {
            feasible
                .traverse(d => registry.fakeDevice(d.platformId).estimatedCurrentQueueWaitMillis.map(w => (w, d)))
                .map(_.minBy(_._1)._2)
        }
    }

    private def chooseHighestFidelityDevice(
        task: QuantumTaskSpec,
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO]
    ): IO[Device] = {
        val feasible = registry.devices.filter(_.qubits >= task.qubits.value)
        if (feasible.isEmpty) {
        new RuntimeException(s"No feasible device for task").raiseError[IO, Device]
        } else {
        feasible
            .traverse(d => Scheduler.estimateFidelity[IO](d, task.circuit, clients, compiler).map(est => (est.logPTotal, d)))
            .map(_.maxBy(_._1)._2)
        }
    }

    def runBaseline(
        policy: BaselinePolicy,
        quantumTasks: List[QuantumTaskSpec],
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO]
    ): IO[BenchmarkRun] =
        for {
            t0 <- monotonicMillis
            metrics <- quantumTasks.traverse{spec => 
                for{
                    id <- ID.make[IO, TaskId]
                    d <- policy match {
                        case BaselinePolicy.LeastBusy => chooseLeastBusyDevice(spec, registry)
                        case BaselinePolicy.HighestFidelity => chooseHighestFidelityDevice(spec, registry, clients, compiler)
                    }
                    rec <- registry.fakeDevice(d.platformId).submitJob(id)
                    est <- Scheduler.estimateFidelity[IO](d, spec.circuit, clients, compiler)
                }yield QuantumTaskMetric(
                    taskId = id,
                    deviceId = d.platformId,
                    queueWaitMillis = rec.queueWaitMillis,
                    predictedLogFidelity = est.logPTotal,
                    predictedSuccessProbability = est.pTotal
                )
            }
            t1 <- monotonicMillis
        } yield BenchmarkRun(
            policyName = policy.name,
            selectedQuantumTasks = quantumTasks.size,
            schedulingWallMillis = t1 - t0,
            quantumMetrics = metrics
        )
}

// object FakeBenchmarkClientsFromRegistry {

//   private def nowIso: String = Instant.now().toString

//   private def providerFromArn(arn: String): String = {
//     val s = arn.toLowerCase
//     if (s.contains("rigetti")) "Rigetti"
//     else if (s.contains("ionq")) "IonQ"
//     else if (s.contains("iqm")) "IQM"
//     else if (s.contains("quera")) "QuEra"
//     else if (s.contains("aqt")) "AQT"
//     else "Unknown"
//   }

//   def make(
//     registry: BenchmarkDeviceRegistry
//   ): FakeBenchmarkClients = {

//     val braketDevicesInRegistry =
//       registry.devicesById.values.toList.filter(_.platform == "Braket")

//     val ibmDevicesInRegistry =
//       registry.devicesById.values.toList.filter(_.platform == "IBM")

//     val alwaysOpenCaps: String =
//      s"""{
//         |  "service": {
//         |    "braketSchemaHeader": { "name": "dummy", "version": "1" },
//         |    "executionWindows": [
//         |      { "executionDay": "Everyday", "windowStartHour": "00:00", "windowEndHour": "23:59" }
//         |    ]
//         |  },
//         |  "paradigm": { "qubitCount": 84 }
//         |}""".stripMargin

//     val braketDeviceListResp: BraketDeviceListResponse =
//       BraketDeviceListResponse(
//         devices = braketDevicesInRegistry.map { d =>
//           BraketDevice(
//             deviceArn = d.platformId,
//             deviceName = d.platformId,
//             deviceCapabilities = alwaysOpenCaps,
//             deviceStatus = "ONLINE",
//             deviceType = "QPU",
//             providerName = providerFromArn(d.platformId)
//           )
//         },
//         nextToken = None
//       )

//     val braketDetailsByArn: Map[String, BraketDeviceDetailsResponse] =
//       braketDevicesInRegistry.map { d =>
//         val arn = d.platformId
//         arn -> BraketDeviceDetailsResponse(
//           deviceArn = arn,
//           deviceName = arn,
//           deviceStatus = "ONLINE",
//           deviceType = "QPU",
//           providerName = providerFromArn(arn),
//           deviceCapabilities = alwaysOpenCaps,
//           deviceQueueInfo = List(
//             BraketDeviceQueueInfo(
//               queue = "QUANTUM_TASKS_QUEUE",
//               queuePriority = None,
//               queueSize = registry.queueLen(arn).toString
//             )
//           )
//         )
//       }.toMap

//     def braketSubmitDummy(req: BraketCreateQuantumTaskRequest, qasm: String): IO[BraketCreateQuantumTaskResponse] =
//       IO.pure(
//         BraketCreateQuantumTaskResponse(
//           quantumTaskArn = s"arn:aws:braket:bench:task/${req.clientToken}"
//         )
//       )

//     def braketGetDummy(taskId: String): IO[BraketQuantumTaskResponse] = {
//       val deviceArn = braketDevicesInRegistry.headOption.map(_.platformId).getOrElse("braket:unknown")
//       val arn = if (taskId.startsWith("arn:")) taskId else s"arn:aws:braket:bench:task/$taskId"
//       IO.pure(
//         BraketQuantumTaskResponse(
//           actionMetadata = BraketActionMetadata(actionType = "OPENQASM", executableCount = 1, programCount = 1),
//           associations = Nil,
//           createdAt = nowIso,
//           deviceArn = deviceArn,
//           deviceParameters = "{}",
//           endedAt = Some(nowIso),
//           experimentalCapabilities = None,
//           failureReason = None,
//           numSuccessfulShots = 0,
//           outputS3Bucket = None,
//           outputS3Directory = None,
//           quantumTaskArn = arn,
//           queueInfo = BraketDeviceQueueInfo("QUANTUM_TASKS_QUEUE", None, "0"),
//           shots = 0,
//           status = "COMPLETED",
//           tags = None
//         )
//       )
//     }


//     val ibmBackends: BackendsResponseV2 =
//       BackendsResponseV2(
//         devices = ibmDevicesInRegistry.map { d =>
//           IBMBackendDevice(
//             name = d.platformId,
//             status = IBMBackendDeviceStatus(name = "online", reason = None),
//             is_simulator = Some(false),
//             qubits = Some(d.qubits),
//             clops = None,
//             processor_type = None,
//             queue_length = 0,
//             performance_metrics = None,
//             wait_time_seconds = Some(IBMBackendDeviceWaitTimeSeconds(average = 0, p50 = 0, p95 = 0))
//           )
//         }
//       )

//     def ibmSubmitDummy(r: SubmitJobRequestV2): IO[CreateJobResponseV2] =
//       IO.pure(
//         CreateJobResponseV2(
//           id = "bench-job-1",
//           backend = ibmDevicesInRegistry.headOption.map(_.platformId).getOrElse("ibm:unknown"),
//           session_id = None,
//           `private` = None,
//           calibration_id = None
//         )
//       )

//     def ibmListJobDummy(id: String): IO[JobDetailsResponseV2] =
//       IO.pure(
//         JobDetailsResponseV2(
//           id = id,
//           backend = ibmDevicesInRegistry.headOption.map(_.platformId).getOrElse("ibm:unknown"),
//           state = JobState(status = "Completed", reason = None, reason_code = None, reason_solution = None),
//           status = "Completed",
//           created = nowIso,
//           program = JobProgram(id = "bench-program"),
//           runtime = None,
//           cost = 0,
//           tags = None,
//           session_id = None,
//           user_id = "bench-user",
//           `private` = None,
//           estimated_running_time_seconds = None,
//           calibration_id = None
//         )
//       )

//     def ibmMetricsDummy(id: String): IO[JobMetricsResponse] =
//       IO.pure(
//         JobMetricsResponse(
//           timestamps = JobTimeStamps(created = nowIso, finished = Some(nowIso), running = Some(nowIso)),
//           bss = JobBSS(seconds = 0),
//           usage = JobUsage(quantum_seconds = 0, seconds = 0),
//           qiskit_version = "bench",
//           estimated_start_time = None,
//           estimated_completion_time = None,
//           position_in_queue = Some(0),
//           position_in_provider = None
//         )
//       )

//     FakeBenchmarkClients(
//       braketDeviceList = IO.pure(braketDeviceListResp),
//       braketDeviceDetails = ids => IO.pure(ids.flatMap(id => braketDetailsByArn.get(id))),
//       braketSubmit = braketSubmitDummy,
//       braketGetJob = braketGetDummy,

//       ibmFetchBearerToken = IO.pure("dummy-token"),
//       ibmDeviceInfo = IO.pure(ibmBackends),
//       ibmSubmit = ibmSubmitDummy,
//       ibmListJob = ibmListJobDummy,
//       ibmMetrics = ibmMetricsDummy
//     )
//   }
// }

object FakeBenchmarkClientsFromRegistry {

  private def nowIso: String = Instant.now().toString

  private def providerFromArn(arn: String): String = {
    val s = arn.toLowerCase
    if (s.contains("rigetti")) "Rigetti"
    else if (s.contains("ionq")) "IonQ"
    else if (s.contains("iqm")) "IQM"
    else if (s.contains("quera")) "QuEra"
    else if (s.contains("aqt")) "AQT"
    else "Unknown"
  }

  def make(
    registry: BenchmarkDeviceRegistry
  ): IO[FakeBenchmarkClients] = {

    val braketDevicesInRegistry =
      registry.devicesById.values.toList.filter(_.platform == "Braket")

    val ibmDevicesInRegistry =
      registry.devicesById.values.toList.filter(_.platform == "IBM")

    val alwaysOpenCaps: String =
     s"""{
        |  "service": {
        |    "braketSchemaHeader": { "name": "dummy", "version": "1" },
        |    "executionWindows": [
        |      { "executionDay": "Everyday", "windowStartHour": "00:00", "windowEndHour": "23:59" }
        |    ]
        |  },
        |  "paradigm": { "qubitCount": 84 }
        |}""".stripMargin

    val braketDeviceListResp: BraketDeviceListResponse =
      BraketDeviceListResponse(
        devices = braketDevicesInRegistry.map { d =>
          BraketDevice(
            deviceArn = d.platformId,
            deviceName = d.platformId,
            deviceCapabilities = alwaysOpenCaps,
            deviceStatus = "ONLINE",
            deviceType = "QPU",
            providerName = providerFromArn(d.platformId)
          )
        },
        nextToken = None
      )

    val braketDetailsByArn: Map[String, BraketDeviceDetailsResponse] =
      braketDevicesInRegistry.map { d =>
        val arn = d.platformId
        arn -> BraketDeviceDetailsResponse(
          deviceArn = arn,
          deviceName = arn,
          deviceStatus = "ONLINE",
          deviceType = "QPU",
          providerName = providerFromArn(arn),
          deviceCapabilities = alwaysOpenCaps,
          deviceQueueInfo = List(
            BraketDeviceQueueInfo(
              queue = "QUANTUM_TASKS_QUEUE",
              queuePriority = None,
              queueSize = registry.queueLen(arn).toString
            )
          )
        )
      }.toMap

    val ibmBackends: BackendsResponseV2 =
      BackendsResponseV2(
        devices = ibmDevicesInRegistry.map { d =>
          IBMBackendDevice(
            name = d.platformId,
            status = IBMBackendDeviceStatus(name = "online", reason = None),
            is_simulator = Some(false),
            qubits = Some(d.qubits),
            clops = None,
            processor_type = None,
            queue_length = registry.queueLen(d.platformId),
            performance_metrics = None,
            wait_time_seconds = Some(
              IBMBackendDeviceWaitTimeSeconds(average = 0, p50 = 0, p95 = 0)
            )
          )
        }
      )

    for {
      braketTaskToDeviceRef <- Ref.of[IO, Map[String, String]](Map.empty)
      ibmJobToBackendRef    <- Ref.of[IO, Map[String, String]](Map.empty)
    } yield {

      def braketSubmitDummy(
        req: BraketCreateQuantumTaskRequest,
        qasm: String
      ): IO[BraketCreateQuantumTaskResponse] = {
        val taskArn = s"arn:aws:braket:bench:task/${req.clientToken}"
        braketTaskToDeviceRef
          .update(_ + (taskArn -> req.deviceArn)) *>
          IO.pure(
            BraketCreateQuantumTaskResponse(
              quantumTaskArn = taskArn
            )
          )
      }

      def braketGetDummy(taskId: String): IO[BraketQuantumTaskResponse] = {
        val arn =
          if (taskId.startsWith("arn:")) taskId
          else s"arn:aws:braket:bench:task/$taskId"

        braketTaskToDeviceRef.get.map { taskMap =>
          val deviceArn = taskMap.getOrElse(arn, "braket:unknown")
          val queueSize =
            if (registry.devicesById.contains(deviceArn)) registry.queueLen(deviceArn).toString
            else "0"

          BraketQuantumTaskResponse(
            actionMetadata = BraketActionMetadata(
              actionType = "OPENQASM",
              executableCount = 1,
              programCount = 1
            ),
            associations = Nil,
            createdAt = nowIso,
            deviceArn = deviceArn,
            deviceParameters = "{}",
            endedAt = Some(nowIso),
            experimentalCapabilities = None,
            failureReason = None,
            numSuccessfulShots = 0,
            outputS3Bucket = None,
            outputS3Directory = None,
            quantumTaskArn = arn,
            queueInfo = BraketDeviceQueueInfo("QUANTUM_TASKS_QUEUE", None, queueSize),
            shots = 0,
            status = "COMPLETED",
            tags = None
          )
        }
      }

      def ibmSubmitDummy(r: SubmitJobRequestV2): IO[CreateJobResponseV2] =
        for {
          jobId <- IO.delay(java.util.UUID.randomUUID().toString)
          _ <- ibmJobToBackendRef.update(_ + (jobId -> r.backend))
        } yield CreateJobResponseV2(
          id = jobId,
          backend = r.backend,
          session_id = None,
          `private` = None,
          calibration_id = None
        )

      def ibmListJobDummy(id: String): IO[JobDetailsResponseV2] =
        ibmJobToBackendRef.get.map { jobs =>
          val backend = jobs.getOrElse(id, "ibm:unknown")
          JobDetailsResponseV2(
            id = id,
            backend = backend,
            state = JobState(
              status = "Completed",
              reason = None,
              reason_code = None,
              reason_solution = None
            ),
            status = "Completed",
            created = nowIso,
            program = JobProgram(id = "bench-program"),
            runtime = None,
            cost = 0,
            tags = None,
            session_id = None,
            user_id = "bench-user",
            `private` = None,
            estimated_running_time_seconds = None,
            calibration_id = None
          )
        }

      def ibmMetricsDummy(id: String): IO[JobMetricsResponse] =
        IO.pure(
          JobMetricsResponse(
            timestamps = JobTimeStamps(
              created = nowIso,
              finished = Some(nowIso),
              running = Some(nowIso)
            ),
            bss = JobBSS(seconds = 0),
            usage = JobUsage(quantum_seconds = 0, seconds = 0),
            qiskit_version = "bench",
            estimated_start_time = None,
            estimated_completion_time = None,
            position_in_queue = Some(0),
            position_in_provider = None
          )
        )

      FakeBenchmarkClients(
        braketDeviceList = IO.pure(braketDeviceListResp),
        braketDeviceDetails = ids => IO.pure(ids.flatMap(id => braketDetailsByArn.get(id))),
        braketSubmit = braketSubmitDummy,
        braketGetJob = braketGetDummy,
        ibmFetchBearerToken = IO.pure("dummy-token"),
        ibmDeviceInfo = IO.pure(ibmBackends),
        ibmSubmit = ibmSubmitDummy,
        ibmListJob = ibmListJobDummy,
        ibmMetrics = ibmMetricsDummy
      )
    }
  }
}
