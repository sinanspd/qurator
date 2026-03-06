package qurator.testbed

import cats._
import cats.effect._
import cats.syntax.all._

import java.time.{Duration, LocalDateTime}

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
      QuantumTaskSpec(Circuit(List(X(0)), 1), TaskQubits(1), TaskShots(1000), TaskDepth(1)),
      QuantumTaskSpec(Circuit(List(H(0)), 1), TaskQubits(1), TaskShots(1000), TaskDepth(1)),
      QuantumTaskSpec(Circuit(List(X(0), H(0)), 1), TaskQubits(1), TaskShots(2000), TaskDepth(2)),
      QuantumTaskSpec(Circuit(List(CX(0, 1)), 2), TaskQubits(2), TaskShots(1500), TaskDepth(1)),
      QuantumTaskSpec(Circuit(List(H(0), CX(0, 1)), 2), TaskQubits(2), TaskShots(1500), TaskDepth(2)),
      QuantumTaskSpec(Circuit(List(X(0), X(1), CZ(0, 1)), 2), TaskQubits(2), TaskShots(3000), TaskDepth(3)),
      QuantumTaskSpec(Circuit(List(X(0), H(1), Swap(0, 1)), 2), TaskQubits(2), TaskShots(2500), TaskDepth(3))
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
  deviceEstimator: DeviceEstimator[IO],
  jobsRef: Ref[IO, Vector[JobRecord]]
) {

  private def nowF: IO[LocalDateTime] =
    Sync[IO].delay(LocalDateTime.now())

  private def processingMillisForDevice: IO[Long] =
    deviceEstimator.estimateDeviceProcessingSpeed(device.platformId).map {
      case (min0, max0) =>
        val lo = math.min(min0, max0).toLong
        val hi = math.max(min0, max0).toLong
        if (hi <= lo) lo else (lo + hi) / 2L
    }

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
      queueLen <- deviceEstimator.estimateDeviceQueueLength(device.platformId)
      procMillis <- processingMillisForDevice

      externalQueueMillis = queueLen.toLong * procMillis
      localQueueMillis = localBacklogMillis(now, existing)
      totalQueueMillis = externalQueueMillis + localQueueMillis

      startAt = now.plusNanos(totalQueueMillis * 1000000L)
      finishAt = startAt.plusNanos(procMillis * 1000000L)

      rec = JobRecord(
        taskId = taskId,
        deviceId = device.platformId,
        submittedAt = now,
        startedAt = startAt,
        finishedAt = finishAt,
        queueWaitMillis = totalQueueMillis,
        runMillis = procMillis
      )

      _ <- jobsRef.update(_ :+ rec)
    } yield rec

  def jobRecord(taskId: TaskId): IO[Option[JobRecord]] =
    jobsRef.get.map(_.find(_.taskId == taskId))

  def estimatedCurrentQueueWaitMillis: IO[Long] =
    for {
      now <- nowF
      existing <- jobsRef.get
      queueLen <- deviceEstimator.estimateDeviceQueueLength(device.platformId)
      procMillis <- processingMillisForDevice
      externalQueueMillis = queueLen.toLong * procMillis
      localQueueMillis = localBacklogMillis(now, existing)
    } yield externalQueueMillis + localQueueMillis
}

object BenchmarkFakeDevice {
  def make(
    device: Device,
    deviceEstimator: DeviceEstimator[IO]
  ): IO[BenchmarkFakeDevice] =
    Ref
      .of[IO, Vector[JobRecord]](Vector.empty)
      .map(ref => new BenchmarkFakeDevice(device, deviceEstimator, ref))
}

final case class BenchmarkDeviceRegistry(
  devicesById: Map[String, Device],
  fakeDevicesById: Map[String, BenchmarkFakeDevice],
  calibrationsById: Map[String, DeviceCalibration]
) {
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
                qubits = 84,
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
            )
        )

    ///saaaaaymmmm 
    def defaultCalibrations: Map[String, DeviceCalibration] =
        Map(
        "braket-rigetti-ankaa" ->
            RigettiCalibration(
            avg1qFidelityPct = 98.8,
            readoutFidelityPct = 92.5,
            swapFidelityPct = 96.5,
            t1Seconds = 8.0,
            t2Seconds = 4.0,
            swapGateDurationNs = 300,
            readoutDurationNs = 1200,
            oneQGateDurationNs = 40,
            twoQGateDurationNs = 140
            ),
        "braket-iqm-garnet" ->
            IQMCalibration(
            typicalDetectionFalsePositive = 0.02,
            typicalDetectionFalseNegative = 0.03,
            typicalVacancyError = Some(0.04),
            typicalFillingError = None,
            typicalAtomLossProbability = Some(0.03),
            t1SingleSec = Some(7.0),
            t2EchoSingleSec = Some(4.0),
            t2SingleSec = Some(3.5)
            )
        )

    def make(
        devices: List[Device],
        calibrationsById: Map[String, DeviceCalibration],
        deviceEstimator: DeviceEstimator[IO]
    ): IO[BenchmarkDeviceRegistry] =
        devices
        .traverse { d =>
            BenchmarkFakeDevice.make(d, deviceEstimator).map(fd => d.platformId -> fd)
        }
        .map { fakePairs =>
            BenchmarkDeviceRegistry(
                devicesById = devices.map(d => d.platformId -> d).toMap,
                fakeDevicesById = fakePairs.toMap,
                calibrationsById = calibrationsById
            )
        }
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


object BenchmarkHttpClients{

    def make(
        registry: BenchmarkDeviceRegistry,
        dummies: FakeBenchmarkClients
    ): HttpClients[IO] = {
        val azure = new AzureQuantumClient[IO]{
            def fetchDeviceInformation: IO[AzureDeviceStatusResponse] = ??? 
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
                    raw.flatMap { case (s1, s2, tid) => 
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
            quantumIdPairs <- specs.traverse(submitOneWorkItem(scheduler, _)).map(_.flatten)
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






    
//     def runSuite(
//         n: Int,
//         seed: Long,
//         taskFactories: Vector[IO[QuantumTask]],
//         bundleFactory: TaskBundleFactory,
//         probe: SchedulerSubmissionProbe,
//         registry: BenchmarkDeviceRegistry,
//         clients: HttpClients[IO],
//         compiler: FakeCompiler[IO],
//         sink: BenchmarkMetricsSink = BenchmarkMetricsSink.noop
//     ): IO[BenchmarkSuiteResult] =
//         for {
//             quantumTasks <- prepareWorkload(n = n, seed = seed, factories = taskFactories)
//             bundles <- quantumTasks.traverse(bundleFactory.wrap)

//             schedulerRun <- runSchedulerBenchmark[IO](
//                 bundles = bundles,
//                 probe = probe,
//                 registry = registry,
//                 clients = clients,
//                 compiler = compiler
//             )

//             leastBusyRun <- runBaseline(
//                 policy = BaselinePolicy.LeastBusy,
//                 quantumTasks = quantumTasks,
//                 registry = registry,
//                 clients = clients,
//                 compiler = compiler
//             )

//             highestFidelityRun <- runBaseline(
//                 policy = BaselinePolicy.HighestFidelity,
//                 quantumTasks = quantumTasks,
//                 registry = registry,
//                 clients = clients,
//                 compiler = compiler
//             )
//             _ <- sink.persist(schedulerRun)
//             _ <- sink.persist(leastBusyRun)
//             _ <- sink.persist(highestFidelityRun)
//         } yield BenchmarkSuiteResult(
//             schedulerRun = schedulerRun,
//             leastBusyRun = leastBusyRun,
//             highestFidelityRun = highestFidelityRun
//         )
// }


// ///////
// final case class BenchmarkSuiteResult(
//     schedulerRun: BenchmarkRun,
//     leastBusyRun: BenchmarkRun,
//     highestFidelityRun: BenchmarkRun
// )
