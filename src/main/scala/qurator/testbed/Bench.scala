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
import qurator.domain.{ProviderJobTiming, ProviderTaskStatus, QuantumJobResult, QuantumResult}
import fs2.io.file.Path
import qurator.domain.cutting.CuttingRequest
import qurator.util.CuttingStrategies.CuttingStrategy


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
  queueLen: Int,
  msPerGate: Long,
  jobsRef: Ref[IO, Vector[JobRecord]]
) {

  private def nowF: IO[LocalDateTime] =
    Sync[IO].delay(LocalDateTime.now())

  private def localBacklogMillis(now: LocalDateTime, existing: Vector[JobRecord]): Long =
    existing.lastOption match {
      case None => 0L
      case Some(last) =>
        if (last.finishedAt.isAfter(now)) Duration.between(now, last.finishedAt).toMillis
        else 0L
    }


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
}

object BenchmarkFakeDevice {
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
  providerJobRecordsRef: Ref[IO, Map[String, JobRecord]],
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

  def observedQueueLength(deviceId: String): IO[Int] =
    fakeDevice(deviceId).estimatedCurrentQueueWaitMillis.map { waitMs =>
      val perJobMs = msPerGate * 100L
      math.max(0L, waitMs / math.max(1L, perJobMs)).toInt
    }

  def recordProviderSubmission(providerJobId: String, deviceId: String): IO[JobRecord] =
    for {
      syntheticTaskId <- ID.make[IO, TaskId]
      rec             <- fakeDevice(deviceId).submitJob(syntheticTaskId)
      _               <- providerJobRecordsRef.update(_ + (providerJobId -> rec))
    } yield rec

  def providerJobRecord(providerJobId: String): IO[Option[JobRecord]] =
    providerJobRecordsRef.get.map(_.get(providerJobId))
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
            Device(
                platform = "Braket",
                platformId = "braket-iqm-garnet",
                qubits = 20,
                t1 = 0f,
                t2 = 0f,
                gateSet = List.empty
            ),
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
                    avg1qFidelityPct = 98.46233903284783, //
                    readoutFidelityPct = 95.70731707317071, //
                    swapFidelityPct = 89.973192441561, //
                    t1Seconds = 3.776451788428128e-5, //
                    t2Seconds = 2.1430702360129197e-5, //
                    swapGateDurationNs = 300,
                    readoutDurationNs = 1200,
                    oneQGateDurationNs = 40,
                    twoQGateDurationNs = 140
                ),
            "braket-iqm-garnet" -> //
                IQMCalibration(
                    t1 = 3.490079494002981e-5,
                    t2 = 8.677614628987359e-6,
                    q1fidelity = 99.90661687868104,
                    q2fidelity = 99.33252648664347,
                    readoutFidelity = 97.89
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
                    readoutFidelity = 99.74,
                    readoutDurationSec = 0.0015,
                    oneQGateDurationSec = 45e-6,
                    oneQGateFidelity = 99.97029166666667,
                    twoQGateDurationSec = 0.000335,
                    twoQGateFidelity = 98.5349
                ),
            "braket-ionq-forte-1" ->  // 
                IonQCalibration(
                    t1Seconds = 100,
                    t2Seconds = 1,
                    avg1qFidelityPct = Double.NaN,
                    avg2qFidelityPct = 98.9,
                    avgReadoutFidelity = 98.65,
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
        msPerGate: Long = 5L,
        seed: Long = 42L
    ): IO[BenchmarkDeviceRegistry] =
        for {
            _ <- Logger[IO].info("Created Benchmark Device Registry")
            byId = devices.map(d => d.platformId -> d).toMap
            rng  = new scala.util.Random(seed)
            qMap = byId.keys.map(id => id -> rng.between(10, 200)).toMap
            fakePairs <- devices.traverse { d =>
                BenchmarkFakeDevice.make(d, qMap(d.platformId), msPerGate).map(fd => d.platformId -> fd)
            }
            providerJobRecordsRef <- Ref.of[IO, Map[String, JobRecord]](Map.empty)
        } yield BenchmarkDeviceRegistry(
            devicesById = byId,
            calibrationsById = calibrationsById,
            queueLenByDeviceId = qMap,
            fakeDevicesById = fakePairs.toMap,
            providerJobRecordsRef = providerJobRecordsRef,
            msPerGate = msPerGate
        )
                
}

final case class SubmittedQuantum(
    taskId: TaskId,
    deviceId: String,
    jobId: String
)

final case class QuantumTaskMetric(
    taskId: TaskId,
    jobId: String,
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

    lazy val uniqueSubmittedJobs: Int =
        quantumMetrics.map(_.jobId).distinct.size

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
            def fetchAvailableDevices: IO[List[Device]] =
                BraketClient.fetchAvailableDevices(fetchDeviceList, fetchDeviceDetails)
            def fetchDeviceDetails(ids: List[String]): IO[List[BraketDeviceDetailsResponse]] = dummies.braketDeviceDetails(ids)
            def submitBraketOpenQasmTask(r: BraketCreateQuantumTaskRequest, qasmSource: String): IO[BraketCreateQuantumTaskResponse] =
                dummies.braketSubmit(r, qasmSource)
            def getQuantumTask(taskId: String) : IO[BraketQuantumTaskResponse] = dummies.braketGetJob(taskId)
            def fetchJobTiming(taskId: String, status: ProviderTaskStatus): IO[ProviderJobTiming] =
                IO.pure(ProviderJobTiming(None, None))
            def fetchTaskResult(taskId: String, status: ProviderTaskStatus): IO[QuantumJobResult] =
                IO.pure(QuantumJobResult.unavailable(provider, taskId, None, "benchmark client does not fetch Braket results"))
            def fetchDeviceCalibration(deviceArn: String): IO[DeviceCalibration] = registry.calibration(deviceArn).pure[IO]
        }

        val ibm = new IBMClient[IO]{
            def fetchBearerToken: IO[String] = dummies.ibmFetchBearerToken
            def fetchDeviceInformation: IO[BackendsResponseV2] = dummies.ibmDeviceInfo
            def fetchAvailableDevices: IO[List[Device]] =
                IBMClient.fetchAvailableDevices(fetchDeviceInformation)
            def fetchDeviceDetails(ids: List[String]): IO[List[IBMBackendDevice]] =
                IBMClient.fetchDeviceDetails(fetchDeviceInformation, ids)
            def createSession(r: CreateSessionRequest): IO[SessionResponse] =
                IO.pure(
                    SessionResponse(
                        id = "bench-session-1",
                        backend_name = r.backend.orElse(r.backend_name),
                        started_at = None,
                        activated_at = None,
                        closed_at = None,
                        last_job_started = None,
                        last_job_completed = None,
                        interactive_ttl = r.interactive_ttl,
                        max_ttl = r.max_ttl,
                        active_ttl = r.active_ttl,
                        state = Some("open"),
                        state_reason = None,
                        accepting_jobs = Some(true),
                        mode = Some(r.mode),
                        timestamps = None,
                        user_id = None,
                        elapsed_time = None
                    )
                )
            def getSession(id: String): IO[SessionResponse] =
                IO.pure(
                    SessionResponse(
                        id = id,
                        backend_name = None,
                        started_at = None,
                        activated_at = None,
                        closed_at = None,
                        last_job_started = None,
                        last_job_completed = None,
                        interactive_ttl = None,
                        max_ttl = None,
                        active_ttl = None,
                        state = Some("open"),
                        state_reason = None,
                        accepting_jobs = Some(true),
                        mode = Some("batch"),
                        timestamps = None,
                        user_id = None,
                        elapsed_time = None
                    )
                )
            def updateSession(id: String, r: UpdateSessionRequest): IO[Unit] = IO.unit
            def closeSession(id: String): IO[Unit] = IO.unit
            def submitJob(r: SubmitJobRequestV2): IO[CreateJobResponseV2] =  dummies.ibmSubmit(r) 
            def listJobDetails(id: String): IO[JobDetailsResponseV2] = dummies.ibmListJob(id)
            def getJobMetrics(id: String): IO[JobMetricsResponse] = dummies.ibmMetrics(id)
            def getJobResults(id: String): IO[String] =
                IO.pure("""{"counts":{"0":1}}""")
            def fetchTaskResult(taskId: String, status: ProviderTaskStatus): IO[QuantumJobResult] =
                getJobResults(taskId).map(raw => IBMClient.jobResultFromRaw(provider, taskId, status, raw))
            def fetchJobTiming(taskId: String, status: ProviderTaskStatus): IO[ProviderJobTiming] =
                IO.pure(ProviderJobTiming(None, None))
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
    
    private def expandSpecForBenchmark(
        spec: QuantumTaskSpec,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO],
        targetEstimatedFidelity: Double,
        cuttingStrategy: CuttingStrategy[IO],
        additionalOptimizationRuns: Circuit => List[Circuit]
    ): IO[List[QuantumTaskSpec]] =
        for {
            devices <- Scheduler.getAvailableDevices[IO](clients)

            feasibleNoCut <- devices
                .filter(_.qubits >= spec.qubits.value)
                .traverse(d => Scheduler.estimateFidelity[IO](d, spec.circuit, clients, compiler))
                .map(_.exists(_.pTotal > targetEstimatedFidelity))

            expanded <-
                if (feasibleNoCut) {
                    List(spec).pure[IO]
                } else {
                    cuttingStrategy(
                        CuttingRequest(
                            circuit = spec.circuit,
                            devices = devices,
                            targetEstimatedFidelity = targetEstimatedFidelity,
                            shots = Some(spec.shots.value.toLong)
                        )
                    ).map { decision =>
                        val cut = decision.selected.subcircuits
                        cut.flatMap(additionalOptimizationRuns).map { c =>
                            QuantumTaskSpec(
                                circuit = c,
                                qubits  = TaskQubits(c.qubits),
                                shots   = spec.shots,
                                depth   = spec.depth
                            )
                        }
                    }
                }
        } yield expanded

    private def submitOneWorkItem(
        scheduler: Scheduler[IO],
        spec: QuantumTaskSpec,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO],
        targetEstimatedFidelity: Double,
        cuttingStrategy: CuttingStrategy[IO],
        additionalOptimizationRuns: Circuit => List[Circuit],
        onQuantumComplete: QuantumResult => IO[Unit]
    ): IO[List[(TaskId, QuantumTaskSpec)]] =
        for {
            npw <- LocalDateTime.now().pure[IO]

            parentReq = NewClassicalTaskRequest(
                program = (),
                parentTasks = Nil,
                childTasks = Nil,
                createdAt = npw
            )

            parentId <- scheduler.submitTask(parentReq)

            expectedExpanded <- expandSpecForBenchmark(
                spec,
                clients,
                compiler,
                targetEstimatedFidelity,
                cuttingStrategy,
                additionalOptimizationRuns
            )

            quantumReq = NewQuantumTaskRequest(
                circuit = spec.circuit,
                qubits = spec.qubits,
                shots = spec.shots,
                depth = spec.depth,
                parentTasks = parentId,
                childTasks = Nil,
                createdAt = npw
            )

            quantumIds <- scheduler.submitTask(quantumReq, onQuantumComplete)

            _ <-
                if (quantumIds.length != expectedExpanded.length)
                    IO.raiseError(
                        new RuntimeException(
                            s"Benchmark expansion mismatch: scheduler returned ${quantumIds.length} ids but benchmark expected ${expectedExpanded.length}"
                        )
                    )
                else IO.unit

            childReq = NewClassicalTaskRequest(
                program = (),
                parentTasks = quantumIds,
                childTasks = Nil,
                createdAt = npw
            )

            _ <- scheduler.submitTask(childReq)
        } yield quantumIds.zip(expectedExpanded)

    private def waitUntilAllCompleted(
        completedRef: Ref[IO, Map[TaskId, QuantumResult]],
        expectedQuantumIds: Set[TaskId],
        pollEvery: scala.concurrent.duration.FiniteDuration
    ): IO[List[QuantumResult]] = {
        def loop: IO[List[QuantumResult]] =
            completedRef.get.flatMap { seen =>
                if (expectedQuantumIds.subsetOf(seen.keySet)) {
                    expectedQuantumIds.toList.traverse { taskId =>
                        seen.get(taskId) match {
                            case Some(result) => result.pure[IO]
                            case None =>
                                new RuntimeException(s"Missing completion event for taskId=$taskId")
                                    .raiseError[IO, QuantumResult]
                        }
                    }
                } else {
                    Temporal[IO].sleep(pollEvery) *> loop
                }
            }

        if (expectedQuantumIds.isEmpty) List.empty[QuantumResult].pure[IO]
        else loop
    }

    private def quantumMetricsForAssignments(
        completions: List[QuantumResult],
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO]
    ): IO[List[QuantumTaskMetric]] =
          completions
            .groupBy(_.jobId)
            .toList
            .traverse { case (jobId, subs) =>
                val deviceId = subs.head.deviceId.getOrElse(
                    throw new RuntimeException(s"Missing deviceId in completion callback for providerJobId=$jobId")
                )
                val device   = registry.device(deviceId)
                val executedCircuit = subs.head.executedCircuit.getOrElse(
                    throw new RuntimeException(s"Missing executedCircuit in completion callback for providerJobId=$jobId")
                )

                for {
                    rec <- registry.providerJobRecord(jobId).flatMap {
                        case Some(r) => r.pure[IO]
                        case None =>
                            new RuntimeException(
                                s"Missing benchmark job record for providerJobId=$jobId, device=$deviceId"
                            ).raiseError[IO, JobRecord]
                    }

                    est <- Scheduler.estimateFidelity[IO](
                        device,
                        executedCircuit,
                        clients,
                        compiler
                    )
                } yield subs.map { sub =>
                    QuantumTaskMetric(
                        taskId = sub.taskId.getOrElse(
                            throw new RuntimeException(s"Missing taskId in quantum result for providerJobId=$jobId")
                        ),
                        jobId = jobId,
                        deviceId = deviceId,
                        queueWaitMillis = rec.queueWaitMillis,
                        predictedLogFidelity = est.logPTotal,
                        predictedSuccessProbability = est.pTotal
                    )
                }
            }
            .map(_.flatten)


    def runSchedulerBenchmark(
        scheduler: Scheduler[IO], 
        specs: List[QuantumTaskSpec],
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        cuttingStrategy: CuttingStrategy[IO],
        compiler: FakeCompiler[IO],
        pollEvery: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.DurationInt(100).millis
    ): IO[BenchmarkRun] = {
        for{
            t0 <- monotonicMillis
            report <- WorkloadSpecs.loadedTasks
            _ <- Logger[IO].info(s"Loaded ${report.size} task(s)")
            _ <- Logger[IO].info("Starting Scheduler Benchmark")
            completedQuantumRef <- Ref.of[IO, Map[TaskId, QuantumResult]](Map.empty)
            onQuantumComplete = (result: QuantumResult) =>
                result.taskId match {
                    case Some(taskId) => completedQuantumRef.update(_ + (taskId -> result))
                    case None         => IO.raiseError(new RuntimeException(s"Missing taskId in quantum result for job=${result.jobId}"))
                }
            quantumIdPairs <- specs.traverse(submitOneWorkItem(scheduler, _, clients, compiler, 0.9, cuttingStrategy, (c: Circuit) => List(c), onQuantumComplete)).map(_.flatten)
            expectedIds = quantumIdPairs.map(_._1).toSet
            completions <- waitUntilAllCompleted(completedQuantumRef, expectedIds, pollEvery)
            t1 <- monotonicMillis
            metrics <- quantumMetricsForAssignments(
                completions,
                registry,
                clients,
                compiler
            )
        } yield BenchmarkRun(
            policyName = "scheduler",
            selectedQuantumTasks = expectedIds.size,
            schedulingWallMillis = t1 - t0,
            quantumMetrics = metrics
        )
    }

    private def chooseLeastBusyDevice(
        task: QuantumTaskSpec,
        clients: HttpClients[IO]
    ): IO[Device] =
    Scheduler.getAvailableDevices[IO](clients).flatMap { devices =>
        val feasible = devices.filter(_.qubits >= task.qubits.value)
        if (feasible.isEmpty)
        IO.raiseError(new RuntimeException("No feasible device for task"))
        else
        IO.pure(feasible.minBy(_.queueLength))
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
            metrics <- quantumTasks.traverse { spec =>
                for {
                    logicalTaskId <- ID.make[IO, TaskId]

                    device <- policy match {
                        case BaselinePolicy.LeastBusy =>
                            chooseLeastBusyDevice(spec, clients)

                        case BaselinePolicy.HighestFidelity =>
                            chooseHighestFidelityDevice(spec, registry, clients, compiler)
                    }
                    _ <- Logger[IO].info(s"Baseline Device: ${device.platformId}")

                    jobId <- IO.delay(s"baseline-${java.util.UUID.randomUUID().toString}")

                    rec <- registry.recordProviderSubmission(
                        providerJobId = jobId,
                        deviceId = device.platformId
                    )

                    est <- Scheduler.estimateFidelity[IO](
                        device,
                        spec.circuit,
                        clients,
                        compiler
                    )

                } yield QuantumTaskMetric(
                    taskId = logicalTaskId,
                    jobId = jobId,
                    deviceId = device.platformId,
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
        for {
          _ <- braketTaskToDeviceRef.update(_ + (taskArn -> req.deviceArn))
          _ <- registry.recordProviderSubmission(taskArn, req.deviceArn)
        } yield BraketCreateQuantumTaskResponse(
          quantumTaskArn = taskArn
        )
      }
      
    def braketGetDummy(taskId: String): IO[BraketQuantumTaskResponse] = {
        val arn =
          if (taskId.startsWith("arn:")) taskId
          else s"arn:aws:braket:bench:task/$taskId"

        for {
          taskMap <- braketTaskToDeviceRef.get
          deviceArn = taskMap.getOrElse(arn, "braket:unknown")
          queueSize <-
            if (registry.devicesById.contains(deviceArn))
              registry.observedQueueLength(deviceArn).map(_.toString)
            else
              "0".pure[IO]
        } yield BraketQuantumTaskResponse(
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

      def ibmSubmitDummy(r: SubmitJobRequestV2): IO[CreateJobResponseV2] =
        for {
          jobId <- IO.delay(java.util.UUID.randomUUID().toString)
          _     <- ibmJobToBackendRef.update(_ + (jobId -> r.backend))
          _     <- registry.recordProviderSubmission(jobId, r.backend)
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
        for {
          jobs <- ibmJobToBackendRef.get
          backend = jobs.getOrElse(id, "ibm:unknown")
          positionInQueue <-
            if (registry.devicesById.contains(backend))
              registry.observedQueueLength(backend)
            else
              0.pure[IO]
        } yield JobMetricsResponse(
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

        FakeBenchmarkClients(
        braketDeviceList = IO.pure(braketDeviceListResp),

        braketDeviceDetails = ids =>
          ids.traverse { arn =>
            registry.observedQueueLength(arn).map { q =>
              BraketDeviceDetailsResponse(
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
                    queueSize = q.toString
                  )
                )
              )
            }
          },

        braketSubmit = braketSubmitDummy,
        braketGetJob = braketGetDummy,

        ibmFetchBearerToken = IO.pure("dummy-token"),

        ibmDeviceInfo =
          ibmDevicesInRegistry.traverse { d =>
            registry.observedQueueLength(d.platformId).map { q =>
              IBMBackendDevice(
                name = d.platformId,
                status = IBMBackendDeviceStatus(name = "online", reason = None),
                is_simulator = Some(false),
                qubits = Some(d.qubits),
                clops = None,
                processor_type = None,
                queue_length = q,
                performance_metrics = None,
                wait_time_seconds = Some(
                  IBMBackendDeviceWaitTimeSeconds(average = 0, p50 = 0, p95 = 0)
                )
              )
            }
          }.map(BackendsResponseV2.apply),

        ibmSubmit = ibmSubmitDummy,
        ibmListJob = ibmListJobDummy,
        ibmMetrics = ibmMetricsDummy
      )
    }
  }
}
