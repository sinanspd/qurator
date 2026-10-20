package qurator.testbed

import cats.effect.{IO, Ref}
import cats.effect.std.Supervisor
import cats.syntax.all._
import java.time.LocalDateTime
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import qurator.domain.{QuantumResult, cutting}
import qurator.domain.Task._
import qurator.domain.DeviceQueueInformation._
import qurator.domain.SubmittedJobData._
import qurator.domain.circuit.Circuit
import qurator.domain.calibration.DeviceCalibration
import qurator.domain.device.Device
import qurator.effects.Background
import qurator.programs.Scheduler
import qurator.service.DataPersistanceService
import qurator.testbed.QuantumPolicyBenchmark._
import qurator.testbed.fidelity.DeviceFidelityEvaluation
import qurator.util.{CuttingStrategies, FidelityEstimator, Qasm3Parser}
import scala.concurrent.duration._

/** Drives the real scheduler with deterministic simulated providers and only quantum inputs.
  * The provider completes callbacks immediately; recorded service/queue time is virtual.
  * Fragment scores deliberately retain the existing benchmark's proxy semantics.
  */
object FullQuantumSchedulerBenchmark {
  final case class Result(rows: Vector[Assignment], cutInputs: Int, mergedJobs: Int,
      originalInputs: Int, providerJobs: Int, batchedJobs: Int)
  private final case class Submitted(candidate: Candidate, queueNs: Long, batched: Boolean)
  private implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private val persistence = new DataPersistanceService[IO] {
    def getDeviceQueueInformationByPage(page: Int): IO[List[DeviceQueueInformation]] = IO.pure(Nil)
    def persistDeviceQueueInformation(rows: List[DeviceQueueInformationCreate]): IO[Unit] = IO.unit
    def fetchQueueInformationAfterDate(date: LocalDateTime, device: String): IO[List[DeviceQueueInformation]] = IO.pure(Nil)
    def getQueueMinAfterDateForDevice(date: LocalDateTime, device: String): IO[Option[DeviceQueueInformation]] = IO.pure(None)
    def getQueueMaxAfterDateForDevice(date: LocalDateTime, device: String): IO[Option[DeviceQueueInformation]] = IO.pure(None)
    def persistSubmittedJobData(data: SubmittedJobDataCreate): IO[Unit] = IO.unit
    def fetchSubmittedJobDataAfterDate(date: LocalDateTime, provider: String,
        deviceId: String): IO[List[SubmittedJobData]] = IO.pure(Nil)
  }

  private def depth(circuit: Circuit): Int = {
    val last = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    circuit.remainingGates.foreach { gate =>
      val qs = qurator.domain.calibration.gateKey(gate)._2
      val next = qs.map(last).maxOption.getOrElse(0) + 1
      qs.foreach(q => last(q) = next)
    }
    last.values.maxOption.getOrElse(1)
  }

  def run(
      circuits: Vector[Circuit],
      devices: Vector[Device],
      initial: Queues,
      targetEstimatedFidelity: Double,
      // Tests can supply a deterministic real cutting decision to exercise fragment accounting.
      cuttingOverride: Option[CuttingStrategies.CuttingStrategy[IO]] = None,
      calibrations: Map[String, DeviceCalibration] = BenchmarkDeviceRegistry.defaultCalibrations
  ): IO[Result] = Supervisor[IO].use { implicit supervisor =>
    implicit val background: Background[IO] = Background.bgInstance[IO]
    for {
      queues <- Ref.of[IO, Queues](initial)
      submissions <- Ref.of[IO, Map[String, Submitted]](Map.empty)
      completions <- Ref.of[IO, Map[TaskId, QuantumResult]](Map.empty)
      decisions <- Ref.of[IO, Vector[cutting.CuttingDecision]](Vector.empty)
      records <- Ref.of[IO, Map[String, JobRecord]](Map.empty)
      registry = BenchmarkDeviceRegistry(devices.map(d => d.platformId -> d).toMap, Map.empty,
        calibrations, initial.view.mapValues(_.jobs).toMap, records)
      base = DummyResponses.benchmarkClientDummies[IO](Nil, Nil, Nil, "", devices.head.platformId)
      dummies = base.copy(
        ibmDeviceInfo = queues.get.map { current =>
          DummyResponses.ibmBackendsResponseV2(devices.toList.map { d =>
            DummyResponses.ibmBackendDevice(d.platformId, d.qubits, current(d.platformId).jobs, statusName = "online")
          })
        },
        ibmSubmit = req => for {
          _ <- IO { require(req.params.pubs.size == 1, "Expected one circuit per provider submission") }
          source <- IO(Qasm3Parser.parse(req.params.pubs.head, "provider-input"))
          device <- IO.fromOption(devices.find(_.platformId == req.backend))(
            new IllegalArgumentException(s"Unknown simulated backend ${req.backend}"))
          compiled <- DeviceFidelityEvaluation.compile(source, device, registry.calibration(req.backend))
          cal = FidelityEstimator.normalizeCalibration(registry.calibration(req.backend))
          estimate = FidelityEstimator.score(compiled, cal)
          serviceNs = Math.addExact(preparationNs,
            Math.multiplyExact(estimate.schedule.get.makespanNs, req.params.shots.getOrElse(shots).toLong))
          queueNs <- queues.modify { current =>
            val q = current(req.backend)
            (current.updated(req.backend, q.enqueue(serviceNs)), q.availableNs)
          }
          candidate = Candidate(device, compiled, cal, estimate.pTotal, estimate.logPTotal,
            Scheduler.estimatedTranspilationMillis(source), serviceNs)
          id <- submissions.modify { current =>
            val id = s"sim-${current.size}"
            (current.updated(id, Submitted(candidate, queueNs, req.session_id.nonEmpty)), id)
          }
        } yield DummyResponses.ibmCreateJobResponseV2(id, req.backend),
        ibmListJob = id => submissions.get.map { current =>
          DummyResponses.ibmJobDetailsResponseV2(id, current(id).candidate.device.platformId)
        }
      )
      clients = BenchmarkHttpClients.make(registry, dummies, batchSubmissionsEnabled = true)
      available <- Scheduler.getAvailableDevices(clients)
      _ <- IO { require(available.map(_.platformId).toSet == devices.map(_.platformId).toSet,
        "Simulated provider did not advertise the complete benchmark fleet") }
      compiler = FakeCompiler.topologyAware[IO](registry)
      nativeCutting = cuttingOverride.getOrElse(CuttingStrategies.hardwareAware[IO](clients, compiler))
      cutter: CuttingStrategies.CuttingStrategy[IO] = request =>
        nativeCutting(request).flatTap { decision =>
          IO.raiseUnless(decision.selected.subcircuits.nonEmpty &&
            decision.selected.subcircuits.forall(c => devices.exists(_.qubits >= c.qubits)))(
            new IllegalArgumentException(s"Cutting plan '${decision.selected.name}' for " +
              s"${request.circuit.name} contains no executable workload or a fragment too wide for the fleet")
          ) *> decisions.update(_ :+ decision)
        }
      scheduler <- Scheduler.make[IO](
        dataPersistanceService = persistence, clients = clients,
        prioritizationStrategy = _.sortBy {
          case task: QuantumTask => task.createdAt
          case _ => throw new IllegalStateException("Only quantum tasks are allowed in this benchmark")
        }, cuttingStrategy = cutter,
        targetEstimatedFidelity = targetEstimatedFidelity,
        additionalOptimizationRuns = c => List(c), compiler = compiler,
        cuttingEnabled = true, mergingEnabled = true, cuttingEffectiveWidthEnabled = true,
        stageQuantumWorkload = true, dashboardEnabled = false)
      inputIds <- circuits.zipWithIndex.traverse { case (circuit, index) =>
        val request = NewQuantumTaskRequest(circuit, TaskQubits(circuit.qubits), TaskShots(shots),
          TaskDepth(depth(circuit)), Nil, Nil, LocalDateTime.of(2000, 1, 1, 0, 0).plusNanos(index))
        scheduler.submitTask(request, (result: QuantumResult) =>
          IO.fromOption(result.taskId)(new IllegalStateException("Completion lacks task ID"))
            .flatMap(id => completions.update(_ + (id -> result))))
      }
      expected = inputIds.flatten.toSet
      _ <- IO { require(expected.nonEmpty, "Full scheduler produced no executable tasks") }
      completed <- scheduler.startRuntime.use { _ =>
        def awaitAll: IO[Map[TaskId, QuantumResult]] = completions.get.flatMap { done =>
          if (expected.subsetOf(done.keySet)) IO.pure(done)
          else IO.sleep(20.millis) *> IO.defer(awaitAll)
        }
        awaitAll.timeout(2.minutes)
      }
      submitted <- submissions.get
      plans <- decisions.get
      rows = inputIds.zipWithIndex.flatMap { case (ids, index) =>
        ids.map { id =>
          val result = completed(id)
          val s = submitted(result.jobId)
          Assignment(Job(index, circuits(index), Vector(s.candidate)), s.candidate, s.queueNs)
        }
      }
      merged = completed.values.toVector.groupBy(_.jobId).count(_._2.size > 1)
    } yield Result(rows, plans.count(_.selected.cuts > 0), merged, circuits.size, submitted.size,
      submitted.values.count(_.batched))
  }
}
