package qurator

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

final case class JobRecord(
  taskId: TaskId,
  deviceId: String,
  submittedAt: LocalDateTime,
  startedAt: LocalDateTime,
  finishedAt: LocalDateTime,
  queueWaitMillis: Long,
  runMillis: Long
) {
  def isComplete(now: LocalDateTime): Boolean =
    !finishedAt.isAfter(now)
}

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

  def submitJob(task: QuantumTask): IO[Unit] =
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
        taskId = task.uuid,
        deviceId = device.platformId,
        submittedAt = now,
        startedAt = startAt,
        finishedAt = finishAt,
        queueWaitMillis = totalQueueMillis,
        runMillis = procMillis
      )

      _ <- jobsRef.update(_ :+ rec)
    } yield ()

  def checkJobStatus(taskId: TaskId): IO[Option[Boolean]] =
    for {
      now <- nowF
      jobs <- jobsRef.get
    } yield jobs.find(_.taskId == taskId).map(_.isComplete(now))

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
  def device(deviceId: String): Device =
    devicesById(deviceId)

  def fakeDevice(deviceId: String): BenchmarkFakeDevice =
    fakeDevicesById(deviceId)

  def availableDevices: List[Device] =
    devicesById.values.toList

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

final case class TaskBundle(
    quantum: QuantumTask,
    submissionChain: List[Task]
)

trait TaskBundleFactory{
    def wrap(quantum: QuantumTask): IO[TaskBundle]
}

final case class SubmittedQuantum(taskId: TaskId, deviceId: String)

trait SchedulerSubmissionProbe{
    def submitAll(tasks: List[Task]): IO[Unit]
    def snapshotSubmittedQuantum: IO[List[SubmittedQuantum]]
}

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

final case class BenchmarkSuiteResult(
    schedulerRun: BenchmarkRun,
    leastBusyRun: BenchmarkRun,
    highestFidelityRun: BenchmarkRun
)

trait BenchmarkMetricsSink{
    def persist(run: BenchmarkRun): IO[Unit]
}

object BenchmarkMetricsSink {
    def noop: BenchmarkMetricsSink =
        new BenchmarkMetricsSink{
            def persist(run: BenchmarkRun): IO[Unit] = ().pure[IO]
        }
}

object SchedulerBenchmarkSuite { 

    private def freshQuantumTask(
        circuit: Circuit,
        qubits: Int,
        shots: Int,
        depth: Int
    ): IO[QuantumTask] =
        for {
            id <- ID.make[IO, TaskId]
            now <- Sync[IO].delay(LocalDateTime.now())
        } yield QuantumTask(
            uuid = id,
            circuit = circuit,
            qubits = TaskQubits(qubits),
            shots = TaskShots(shots),
            depth = TaskDepth(depth),
            parentTasks = List.empty,
            childTasks = List.empty,
            createdAt = now
        )

    //test only 
    def defaultQuantumTaskFactories: Vector[IO[QuantumTask]] =
        Vector(
            freshQuantumTask(
                circuit = Circuit(List(X(0)), 1),
                qubits = 1,
                shots = 1000,
                depth = 1
            ),
            freshQuantumTask(
                circuit = Circuit(List(H(0)), 1),
                qubits = 1,
                shots = 1000,
                depth = 1
            ),
            freshQuantumTask(
                circuit = Circuit(List(X(0), H(0)), 1),
                qubits = 1,
                shots = 2000,
                depth = 2
            ),
            freshQuantumTask(
                circuit = Circuit(List(CX(0, 1)), 2),
                qubits = 2,
                shots = 1500,
                depth = 1
            ),
            freshQuantumTask(
                circuit = Circuit(List(H(0), CX(0, 1)), 2),
                qubits = 2,
                shots = 1500,
                depth = 2
            ),
            freshQuantumTask(
                circuit = Circuit(List(X(0), X(1), CZ(0, 1)), 2),
                qubits = 2,
                shots = 3000,
                depth = 3
            ),
            freshQuantumTask(
                circuit = Circuit(List(X(0), H(1), Swap(0, 1)), 2),
                qubits = 2,
                shots = 2500,
                depth = 3
            )
        )

    // sample with replacement, each sample gets unique id 
    def prepareWorkload(
        n: Int,
        seed: Long,
        factories: Vector[IO[QuantumTask]]
    ): IO[List[QuantumTask]] =
        if (n <= 0 || factories.isEmpty) List.empty[QuantumTask].pure[IO]
        else {
            val size = factories.size
            Sync[IO].delay(new scala.util.Random(seed)).flatMap { rng =>
                List
                .fill(n)(rng.nextInt(size))
                .traverse(i => factories(i))
            }
        }

    private def monotonicMillis: IO[Long] =
        Temporal[IO].monotonic.map(_.toMillis)
    
    private def waitUntilSubmitted(
        expectedQuantumIds: Set[TaskId],
        probe: SchedulerSubmissionProbe,
        pollEvery: scala.concurrent.duration.FiniteDuration
    ): IO[List[SubmittedQuantum]] = {
        def loop: IO[List[SubmittedQuantum]] =
            probe.snapshotSubmittedQuantum.flatMap { submitted =>
                val submittedIds = submitted.map(_.taskId).toSet
                if (expectedQuantumIds.subsetOf(submittedIds)) submitted.pure[IO]
                else Temporal[IO].sleep(pollEvery) *> loop
            }

        if (expectedQuantumIds.isEmpty) List.empty[SubmittedQuantum].pure[IO]
        else loop
    }

    private def quantumMetricsForAssignments(
        assignments: List[SubmittedQuantum],
        quantumById: Map[TaskId, QuantumTask],
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO]
    ): IO[List[QuantumTaskMetric]] =
        assignments.traverse { sub =>
        val task = quantumById(sub.taskId)
        val device = registry.device(sub.deviceId)

        for {
            est <- Scheduler.estimateFidelity[IO](device, task.circuit, clients, compiler)
            recOpt <- registry.fakeDevice(sub.deviceId).jobRecord(sub.taskId)
            rec <- recOpt match {
            case Some(r) => r.pure[IO]
            case None =>
                new RuntimeException(
                s"Missing JobRecord for task=${sub.taskId} device=${sub.deviceId}"
                ).raiseError[IO, JobRecord]
            }
        } yield QuantumTaskMetric(
            taskId = sub.taskId,
            deviceId = sub.deviceId,
            queueWaitMillis = rec.queueWaitMillis,
            predictedLogFidelity = est.logPTotal,
            predictedSuccessProbability = est.pTotal
        )}

    private def chooseLeastBusyDevice(
        task: QuantumTask,
        available: List[Device],
        registry: BenchmarkDeviceRegistry
    ): IO[Device] = {
        val feasible = available.filter(_.qubits >= task.qubits.value)
        if (feasible.isEmpty) {
        new RuntimeException(s"No feasible device for task=${task.uuid}").raiseError[IO, Device]
        } else {
        feasible
            .traverse(d => registry.fakeDevice(d.platformId).estimatedCurrentQueueWaitMillis.map(w => (w, d)))
            .map(_.minBy(_._1)._2)
        }
    }

    private def chooseHighestFidelityDevice(
        task: QuantumTask,
        available: List[Device],
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO]
    ): IO[Device] = {
        val feasible = available.filter(_.qubits >= task.qubits.value)
        if (feasible.isEmpty) {
        new RuntimeException(s"No feasible device for task=${task.uuid}").raiseError[IO, Device]
        } else {
        feasible
            .traverse(d => Scheduler.estimateFidelity[IO](d, task.circuit, clients, compiler).map(est => (est.logPTotal, d)))
            .map(_.maxBy(_._1)._2)
        }
    }

    def runBaseline(
        policy: BaselinePolicy,
        quantumTasks: List[QuantumTask],
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO]
    ): IO[BenchmarkRun] =
        for {
        available <- Scheduler.getAvailableDevices[IO](clients)
        t0 <- monotonicMillis

        assignments <- quantumTasks.traverse { q =>
            val choose: IO[Device] =
            policy match {
                case BaselinePolicy.LeastBusy =>
                chooseLeastBusyDevice(q, available, registry)
                case BaselinePolicy.HighestFidelity =>
                chooseHighestFidelityDevice(q, available, clients, compiler)
            }

            choose.flatMap { d =>
            registry.fakeDevice(d.platformId).submitJob(q) *>
                SubmittedQuantum(taskId = q.uuid, deviceId = d.platformId).pure[IO]
            }
        }

        t1 <- monotonicMillis
        metrics <- quantumMetricsForAssignments(
            assignments = assignments,
            quantumById = quantumTasks.map(q => q.uuid -> q).toMap,
            registry = registry,
            clients = clients,
            compiler = compiler
        )
        } yield BenchmarkRun(
            policyName = policy.name,
            selectedQuantumTasks = quantumTasks.size,
            schedulingWallMillis = t1 - t0,
            quantumMetrics = metrics
        )

    def runSchedulerBenchmark[F[_]: Temporal: GenUUID: Monad](
        bundles: List[TaskBundle],
        probe: SchedulerSubmissionProbe,
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO],
        pollEvery: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.DurationInt(100).millis
    ): IO[BenchmarkRun] = {
        val quantumById = bundles.map(b => b.quantum.uuid -> b.quantum).toMap
        val expectedIds = quantumById.keySet
        for {
            t0 <- monotonicMillis
            _ <- bundles.traverse_(b => probe.submitAll(b.submissionChain))
            submitted <- waitUntilSubmitted(expectedIds, probe, pollEvery)
            t1 <- monotonicMillis
            metrics <- quantumMetricsForAssignments(
                assignments = submitted.filter(s => expectedIds.contains(s.taskId)),
                quantumById = quantumById,
                registry = registry,
                clients = clients,
                compiler = compiler
            )
        } yield BenchmarkRun(
            policyName = "scheduler",
            selectedQuantumTasks = bundles.size,
            schedulingWallMillis = t1 - t0,
            quantumMetrics = metrics
        )
    }
    
    def runSuite(
        n: Int,
        seed: Long,
        taskFactories: Vector[IO[QuantumTask]],
        bundleFactory: TaskBundleFactory,
        probe: SchedulerSubmissionProbe,
        registry: BenchmarkDeviceRegistry,
        clients: HttpClients[IO],
        compiler: FakeCompiler[IO],
        sink: BenchmarkMetricsSink = BenchmarkMetricsSink.noop
    ): IO[BenchmarkSuiteResult] =
        for {
            quantumTasks <- prepareWorkload(n = n, seed = seed, factories = taskFactories)
            bundles <- quantumTasks.traverse(bundleFactory.wrap)

            schedulerRun <- runSchedulerBenchmark[IO](
                bundles = bundles,
                probe = probe,
                registry = registry,
                clients = clients,
                compiler = compiler
            )

            leastBusyRun <- runBaseline(
                policy = BaselinePolicy.LeastBusy,
                quantumTasks = quantumTasks,
                registry = registry,
                clients = clients,
                compiler = compiler
            )

            highestFidelityRun <- runBaseline(
                policy = BaselinePolicy.HighestFidelity,
                quantumTasks = quantumTasks,
                registry = registry,
                clients = clients,
                compiler = compiler
            )
            _ <- sink.persist(schedulerRun)
            _ <- sink.persist(leastBusyRun)
            _ <- sink.persist(highestFidelityRun)
        } yield BenchmarkSuiteResult(
            schedulerRun = schedulerRun,
            leastBusyRun = leastBusyRun,
            highestFidelityRun = highestFidelityRun
        )
}
