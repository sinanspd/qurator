package qurator.programs

import cats.MonadThrow
import cats.effect._
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import qurator.service.DataPersistanceService
import scala.util.Random
import qurator.domain.Task._
import qurator.effects.GenUUID
import qurator.domain.circuit._
import cats.effect.kernel.Ref
import qurator.domain.ID
import scala.concurrent.duration._
import qurator.domain.device.Device
import java.time._
import scala.math.{abs, exp, log, min}
import cats.effect.kernel.Clock
import qurator.domain.DeviceQueueInformation._
import qurator.modules.HttpClients
import qurator.testbed.FakeCompiler
import qurator.util.FidelityEstimator
import qurator.domain.calibration._
import qurator.domain.Braket._
import qurator.effects.Background
import qurator.util.Retry
import retry.RetryPolicies._
import retry.RetryPolicy
import cats.Monad
import fs2.Stream
import org.typelevel.log4cats.slf4j.Slf4jLogger
import qurator.domain.IBM._
import cats.Applicative
import java.util.UUID
import qurator.dashboard._
import qurator.domain.ProviderClient
import qurator.domain.ProviderTaskStatus
import qurator.domain.ProviderJobTiming
import qurator.domain.QuantumJobResult
import qurator.domain.SubmittedJobData._
import qurator.Types.AppEnvironment


final case class SubmittedJobInfo(
    deviceId: String,
    executedCircuit: Circuit,
    submittedAt: LocalDateTime
)

trait Scheduler[F[_]]{
    def submitTask(taskReq: TaskRequest): F[List[TaskId]]
    def submitTask(taskReq: TaskRequest, onComplete: TaskCompletion => F[Unit]): F[List[TaskId]]
    def estimateQueueTime(device: Device, task: QuantumTask) : F[Long]
    def getSubmittedTasks(): F[List[(String, String, String, TaskId)]]
    def getSubmittedJobInfo(): F[Map[String, SubmittedJobInfo]]
    def startRuntime: Resource[F, Unit]
    def dashboardUrl: String
}

object Scheduler{
  def make[F[_]: GenUUID: Concurrent: Logger : Temporal : Background : Async](
        dataPersistanceService: DataPersistanceService[F],
        clients: HttpClients[F],
        prioritizationStrategy: List[Task] => List[Task],
        cuttingStrategy: (Circuit, List[Device]) => F[List[Circuit]],
        targetEstimatedFidelity: Double, 
        additionalOptimizationRuns: Circuit => List[Circuit],
        dashboardConfig: SchedulerDashboardConfig = SchedulerDashboardConfig(),
        environment: AppEnvironment = AppEnvironment.Development,
        compiler: FakeCompiler[F] //abstract this
  ): F[Scheduler[F]] =
    for {
      readyTasks       <- Ref.of[F, List[Task]](List.empty)
      pendingTasks     <- Ref.of[F, List[Task]](List.empty)
      submittedTasks   <- Ref.of[F, List[(String, String, String, TaskId)]](List.empty) //platform, platformId, jobId, taskId
      completedTasks   <- Ref.of[F, Set[TaskId]](Set.empty)
      taskCallbacks    <- Ref.of[F, Map[TaskId, TaskCompletion => F[Unit]]](Map.empty)
      taskIndex        <- Ref.of[F, Map[TaskId, Task]](Map.empty)
      mergedAliases    <- Ref.of[F, Map[TaskId, List[TaskId]]](Map.empty)
      submittedJobInfo <- Ref.of[F, Map[String, SubmittedJobInfo]](Map.empty)
      dashboardState   <- Ref.of[F, SchedulerDashboardState](SchedulerDashboardState.empty)
      _ <- Logger[F].info("Creating The Scheduler")    
    } yield new Scheduler[F] {

        def dashboardUrl: String =
            SchedulerDashboard.dashboardUrl(dashboardConfig)

        private val idleDelay: FiniteDuration = 250.millis


        //////////////////////////////////////////////////////// ////////////////////////////////////////////////////////
        //TODO 1 Need callback logic
        //TODO 2 batch submissions 
        //TODO 3 We need to move some of the logic to supervisor so that the scheduler keeps running on error 

        //TODO 4 consider impact of cross talk when scheduling multiple tasks on the same device --> need topology aware mapping. Defined as avg distance between data qubits 

        //TODO 5 Estimate preperation time and add to queue time (and use entanglement estimation for runtime estimation)
        //TODO 6 Use estimateSynronizationCost to implement merging. Downside, this requires time estimation for classical tasks.
        //TODO 7 Merge Cut Task Results --> change UI as well

        //TODO 8 Is anything from Q-Dream useful??
        //TODO 9 Is anything from Qonductor useful??
        //TODO 10 Is anything from Pilot-Quantum useful??



        private val mergeEnabled: Boolean = true
        private val mergeMaxQubits: Int = 10
        private val mergeQueueFactorMillis: Long = 3000L
        private val productionMode: Boolean = environment.isProduction
        private val submittedJobDataLookback: FiniteDuration = 1.hour

        private def registerDashboardTask(task: Task, pendingReason: String): F[Unit] =
            dashboardState.update(st => SchedulerDashboard.recordTask(st, task, pendingReason))

        private def markDashboardPendingReason(taskIds: List[TaskId], pendingReason: String): F[Unit] =
            dashboardState.update(st => SchedulerDashboard.markPendingReason(st, taskIds, pendingReason))

        private def markDashboardSubmitted(
            taskIds: List[TaskId],
            provider: Option[String],
            deviceId: Option[String],
            jobId: Option[String],
            note: Option[String] = None
        ): F[Unit] =
            nowMillis.flatMap { ts =>
                dashboardState.update(st =>
                SchedulerDashboard.markSubmitted(
                    st,
                    taskIds = taskIds,
                    provider = provider,
                    deviceId = deviceId,
                    jobId = jobId,
                    submittedAtMillis = ts,
                    note = note
                )
                )
            }

        private def noopCallback(t: TaskCompletion): F[Unit] =
            Applicative[F].unit

        private def rememberCallbacks(
            taskIds: List[TaskId],
            onComplete: TaskCompletion => F[Unit]
        ): F[Unit] =
            taskCallbacks.update(_ ++ taskIds.map(_ -> onComplete).toMap)

        def submitTask(taskReq: TaskRequest): F[List[TaskId]] =
            submitTask(taskReq, noopCallback)

        def submitTask(taskReq: TaskRequest, onComplete: TaskCompletion => F[Unit]): F[List[TaskId]] = taskReq match{
            case str : SynronizedQuantumTaskRequest => 
                Logger[F].info("Received Synronized Task") *> submitSynronizedTaskRequest(str, onComplete)
            case ntr : NewQuantumTaskRequest => 
                 Logger[F].info("Received Quantum Task") *>  submitNewTaskRequest(ntr, onComplete)
            case ctr : NewClassicalTaskRequest => 
                 Logger[F].info("Received Classical Task") *>  submitClassicalTaskRequest(ctr, onComplete)
        }

        private def submitClassicalTaskRequest(
            taskReq: NewClassicalTaskRequest,
            onComplete: TaskCompletion => F[Unit]
        ): F[List[TaskId]] = 
            ID.make[F, TaskId].flatMap(taskId => {
                val t = ClassicalTask( 
                        uuid = taskId,
                        program = taskReq.program,
                        parentTasks = taskReq.parentTasks,
                        childTasks = taskReq.childTasks,
                        createdAt = taskReq.createdAt
                )
                rememberCallbacks(List(t.uuid), onComplete) *>
                rememberTask(t) *> allParentResultsAvailable(t).flatMap{ apr =>
                    val readyNow = taskReq.parentTasks.isEmpty || apr
                    registerDashboardTask(
                        t,
                        if (readyNow) "ready" else "waiting_on_dependencies"
                    ) *>
                    (if(taskReq.parentTasks.isEmpty || apr){
                        enqueueReady(List(t)) *> List(t.uuid).pure[F]

                    }else{
                        enqueuePending(List(t)) *> List(t.uuid).pure[F]
                    })
                }
            })

        private def submitNewTaskRequest(
            taskReq: NewQuantumTaskRequest,
            onComplete: TaskCompletion => F[Unit]
        ): F[List[TaskId]] =  // TST
             for{
                devices <- Scheduler.getAvailableDevices[F](clients)
                needsToBeCut <- requiresCutting(taskReq, devices)
                _ <- Logger[F].info(s"Processing New Quantum Task With ${taskReq.qubits} Qubits, it requries cutting?: $needsToBeCut")
                tids <- 
                    if(needsToBeCut){ 
                        for {
                            cut <- cuttingStrategy(taskReq.circuit, devices)
                            _ <- Logger[F].info(s"Cut length ${cut.length}")
                            optimized = cut.flatMap(additionalOptimizationRuns(_))
                            recreatedTasks <- optimized.traverse { c =>
                                ID.make[F, TaskId].map { taskId =>
                                    QuantumTask(
                                        taskId,
                                        c,
                                        TaskQubits(c.qubits),
                                        taskReq.shots,
                                        taskReq.depth,
                                        taskReq.parentTasks,
                                        taskReq.childTasks,
                                        taskReq.createdAt
                                    )
                                }
                            }
                             _ <- rememberTasks(recreatedTasks)
                             _ <- rememberCallbacks(recreatedTasks.map(_.uuid), onComplete)
                             _ <- recreatedTasks.traverse_(t =>
                                registerDashboardTask(
                                    t,
                                    if (taskReq.parentTasks.nonEmpty) "waiting_on_dependencies" else "ready"
                                )
                            )
                            tids <-
                                if (taskReq.parentTasks.nonEmpty)
                                    enqueuePendingQuantumTasksAndMaybeMerge(recreatedTasks) *> List(recreatedTasks.map(_.uuid): _*).pure[F]
                                else
                                    enqueueReady(recreatedTasks) *> List(recreatedTasks.map(_.uuid): _*).pure[F]
                        } yield tids
                    }else{
                        ID.make[F, TaskId].flatMap(taskId => {
                            val t = QuantumTask(
                                    uuid = taskId,
                                    circuit = taskReq.circuit,
                                    qubits = taskReq.qubits,
                                    shots = taskReq.shots,
                                    depth = taskReq.depth,
                                    parentTasks = taskReq.parentTasks,
                                    childTasks = taskReq.childTasks,
                                    createdAt = taskReq.createdAt
                            )
                            rememberCallbacks(List(t.uuid), onComplete) *>
                            rememberTask(t) *> 
                            registerDashboardTask(
                                t,
                                if (taskReq.parentTasks.nonEmpty) "waiting_on_dependencies" else "ready"
                            ) *>                      
                            (if(taskReq.parentTasks.nonEmpty){
                                enqueuePendingQuantumTasksAndMaybeMerge(List(t)) *> 
                                 (readyTasks.get, pendingTasks.get).tupled.flatMap { case (r, p) =>
                                        Logger[F].info(s"enqueuePending: ready=${r.size}, pending=${p.size}") } *>
                                List(t.uuid).pure[F]
                            } else {
                                enqueueReady(List(t)) *>  
                                (readyTasks.get, pendingTasks.get).tupled.flatMap { case (r, p) =>
                                    Logger[F].info(s"enqueuePending: ready=${r.size}, pending=${p.size}") } *>
                                 List(t.uuid).pure[F]})
                        })
                    }
            } yield tids 

        private def submitSynronizedTaskRequest(
            str: SynronizedQuantumTaskRequest,
            onComplete: TaskCompletion => F[Unit]
        ): F[List[TaskId]] =  // TST
            for{
                devices <- Scheduler.getAvailableDevices[F](clients)
                cutTasks <- 
                    if (str.cut) {
                        str.l.traverse { req =>
                        requiresCutting(req, devices).flatMap { needsToBeCut =>
                            if (needsToBeCut) {
                                cuttingStrategy(req.circuit, devices).map { cutCircuits =>
                                    val optimizedCircuits = cutCircuits.flatMap(additionalOptimizationRuns) 

                                    optimizedCircuits.map { c =>
                                        NewQuantumTaskRequest(
                                            circuit     = c,
                                            qubits      = req.qubits,
                                            shots       = req.shots,
                                            depth       = req.depth,
                                            parentTasks = req.parentTasks,
                                            childTasks  = req.childTasks,
                                            createdAt   = req.createdAt
                                        )
                                    }
                                }
                            } else {
                                List(req).pure[F]
                            }
                        }
                        }.map(_.flatten)
                    } else {
                        str.l.pure[F]
                    }
                tasks <- cutTasks.traverse{req => 
                    ID.make[F, TaskId].map{tid =>
                        QuantumTask(
                            tid,
                            req.circuit, 
                            req.qubits,
                            req.shots,
                            req.depth,
                            req.parentTasks,
                            req.childTasks,
                            req.createdAt
                        )    
                    }
                }
                _ <- rememberTasks(tasks)
                _ <- tasks.traverse_(t =>
                    registerDashboardTask(
                        t,
                        if (t.parentTasks.nonEmpty) "waiting_on_dependencies" else "ready"
                    )
                )
                _ <- Logger[F].info(s"Attempting Merge Sync Tasks")
                possiblyMergedTasks <- Scheduler.attemptToMergeSyncTasks(tasks, clients, compiler, targetEstimatedFidelity) 
                _ <- Logger[F].info(
                    s"Merge attempt: original=${tasks.size}, after=${possiblyMergedTasks.size}, " +
                    s"ids=${possiblyMergedTasks.map(_.uuid).mkString(", ")}"
                )
                _ <- rememberCallbacks(possiblyMergedTasks.map(_.uuid), onComplete)
                groupId <- ID.make[F, TaskId]
                sg = SyncronizedQuantumTaskList(
                    groupId,
                    possiblyMergedTasks,
                    str.t1Budget,
                    LocalDateTime.now()
                )
                _ <- rememberTasks(List(sg))
                allParents = str.l.foldLeft(List.empty[TaskId])((a, b) => a ++ b.parentTasks)
                _ <- if(allParents.isEmpty){enqueueReady(List(sg))}else{enqueuePending(List(sg))}
            }yield possiblyMergedTasks.map(_.uuid)


        def startScheduling(): F[Unit] = {
            def pickNextReady: F[Option[Task]] =
                readyTasks.modify {
                    case h :: t => (t, Some(h))
                    case Nil    => (Nil, None)
                }
                
            def scheduleOnce(task: Task): F[Unit] = task match{
                case ct: ClassicalTask => fakeClassicalTaskScheduler(ct)
                case qt: QuantumTask => scheduleOneQuantumTask(qt)
                case sgt: SyncronizedQuantumTaskList => scheduleSynronizedTasks(sgt)
            }


            def loop: F[Unit] =
                pickNextReady.flatMap {
                    case Some(t) =>
                        scheduleOnce(t).handleErrorWith { e =>
                            Logger[F].error(e)(s"Failed scheduling task=${t.uuid}; re-queueing") *>
                                enqueueReady(List(t)) *>
                                Temporal[F].sleep(idleDelay)
                        } *> loop

                    case None =>
                        Temporal[F].sleep(idleDelay) *> loop
                }

            loop
        }

       private def deviceKey(device: Device): (String, String) =
            (device.platform, device.platformId)

       private def deviceKeyString(key: (String, String)): String =
            s"${key._1}/${key._2}"

       private def scheduleOneQuantumTask(
            task: QuantumTask,
            blacklistedDevices: Set[(String, String)] = Set.empty
        ): F[Unit] =
            for {
                devices <- Scheduler.getAvailableDevices(clients)

                _ <- Logger[F].info(s"Attempting to schedule quantum task ${task.uuid}")
                _ <- Logger[F].info(s"Devices ${devices.map(d => d.platformId)}")
                _ <-
                    if (blacklistedDevices.nonEmpty)
                        Logger[F].info(
                            s"Skipping failed devices for task=${task.uuid}: ${blacklistedDevices.map(deviceKeyString).mkString(", ")}"
                        )
                    else Applicative[F].unit

                eligibleDevices = devices.filter(d => d.qubits >= task.qubits.value)
                suitableDevices = eligibleDevices.filterNot(d => blacklistedDevices.contains(deviceKey(d)))

                _ <- suitableDevices match {
                case Nil =>
                    if (eligibleDevices.nonEmpty && blacklistedDevices.nonEmpty) {
                        val attempted =
                            blacklistedDevices.map(deviceKeyString).mkString(", ")

                        Logger[F].error(
                            s"All eligible devices failed for task=${task.uuid}; attempted=$attempted"
                        ) *>
                            new RuntimeException(s"All eligible devices failed for task=${task.uuid}")
                                .raiseError[F, Unit]
                    } else {
                        Logger[F].warn(s"No eligible devices for task=${task.uuid}")
                    }

                case ds =>
                    for {
                        observedQueueByDevice <- observedQueueMillisByDevice(ds)
                        observedFleetMeanQueue =
                            observedQueueByDevice.values.flatten.toList match {
                                case Nil => None
                                case xs  => Some(xs.sum.toDouble / xs.size.toDouble)
                            }
                        scored <- ds.traverse { d =>
                            (
                                estimateFidelity(d, task.circuit, clients, compiler),
                                estimateTranspilationTime(task.circuit, d.gateSet)
                            ).mapN { (f, tMillis) =>
                                val ac =
                                    getAssignmentCoefficient(
                                        pTotal = f.pTotal,
                                        queueLength = d.queueLength.toLong,
                                        transpileMillis = tMillis,
                                        fleetMeanQueue =  ds.map(_.queueLength.toDouble).sum / ds.size.toDouble,
                                        observedQueueMillis = observedQueueByDevice.getOrElse(d, None),
                                        observedFleetMeanQueueMillis = observedFleetMeanQueue
                                    )

                                (d, ac, f, d.queueLength, tMillis)
                            }
                        }

                        best = scored.maxBy { case (_, ac, f, q, t) => (ac, f.logPTotal, -q, -t) }
                        _ <- Logger[F].info(s"Picked Device Coefficient: $best")
                        (bestDevice, _, _, _, _) = best

                        // compiled <- ???

                        submittedAt <- nowUtcLocalDateTime
                        _ <- submitQuantumToProvider(bestDevice, task, task.circuit).attempt.flatMap {
                            case Right(jobId) =>
                                for {
                                    _ <- submittedJobInfo.update(_ + (jobId -> SubmittedJobInfo(bestDevice.platformId, task.circuit, submittedAt)))
                                    logicalIds <- logicalIdsFor(task.uuid)
                                    _ <- submittedTasks.update(
                                        _ ++ logicalIds.map(tid => (bestDevice.platform, bestDevice.platformId, jobId, tid))
                                    )
                                    _ <- markDashboardSubmitted(
                                        logicalIds,
                                        provider = Some(bestDevice.platform),
                                        deviceId = Some(bestDevice.platformId),
                                        jobId = Some(jobId),
                                        note =
                                        if (logicalIds.size > 1)
                                            Some(s"Merged physical execution of ${logicalIds.size} logical tasks")
                                        else None
                                    )
                                } yield ()

                            case Left(e) =>
                                val updatedBlacklist = blacklistedDevices + deviceKey(bestDevice)

                                Logger[F].warn(e)(
                                    s"Submission failed for task=${task.uuid} on device=${bestDevice.platform}/${bestDevice.platformId}; trying another device"
                                ) *>
                                    scheduleOneQuantumTask(task, updatedBlacklist)
                        }
                    } yield ()
                }
            } yield ()

        private def submitQuantumToProvider(
            device: Device,
            task: QuantumTask,
            compiled: Circuit
        ): F[String] =
            clients.providerClient(device.platform) match {
                case Some(providerClient) =>
                    Logger[F].info(s"Submitting task ${task.uuid} to device $device via ${providerClient.provider}") *>
                        ProviderClient.submitQuantumTask(providerClient, device, task, compiled)

                case None if device.platform == "Azure" =>
                    new RuntimeException("Azure submit not wired in scheduleOneQuantumTask yet").raiseError[F, String]

                case None =>
                    new RuntimeException(s"No ProviderClient registered for platform=${device.platform}").raiseError[F, String]
            }

        def getSubmittedJobInfo(): F[Map[String, SubmittedJobInfo]] =  submittedJobInfo.get


        private def scheduleSynronizedTasks(s: SyncronizedQuantumTaskList): F[Unit] =
            for{
                _ <- Logger[F].info("Scheduling Syncronized Task")
                devices <- Scheduler.getAvailableDevices(clients)
                orderedTasks = prioritizationStrategy(s.tasks).collect{ case t: QuantumTask => t}
                observedQueueByDevice <- observedQueueMillisByDevice(devices)
                candidateDevicesByTask <- orderedTasks.traverse{t =>
                    val suitableDevices = devices.filter(d => d.qubits >= t.qubits.value)
                    suitableDevices.traverse{d =>
                        (estimateFidelity(d, t.circuit, clients, compiler), estimateTranspilationTime(t.circuit, d.gateSet), estimateRunTime(d,t))
                            .mapN{(f, t, run) =>
                               val queueMillis = observedQueueByDevice.getOrElse(d, None).getOrElse(d.queueLength.toLong) + t
                               CandidateDevice(d, fidelity = f.logPTotal, queueMillis = queueMillis, runMillis = run)
                            }
                    }
                    .map{cs =>
                        val possible = cs.filter(_.fidelity >= math.log(targetEstimatedFidelity))
                        if (possible.nonEmpty) possible else cs 
                    }.map(t -> _)   
                }.map(_.toMap)
                _ <- Logger[F].info("Building Sync Plan")
                plan <- buildGreedySynchronizedPlan(
                    orderedTasks,
                    candidateDevicesByTask,
                    s.t1Budget
                )
                _ <- Logger[F].info(s"Built Sync Plan. Plan Length: ${plan.assignments.toList.length}")
                _ <- plan.assignments.toList.traverse_{case (device, tasksOnDevice) => 
                  tasksOnDevice.traverse_{t => 
                    //submitJobWithFallback(device, t, candidateDevicesByTask.getOrElse(t, Nil))  
                    nowUtcLocalDateTime.flatMap { submittedAt =>
                    submitQuantumToProvider(device, t, t.circuit).flatMap { jobId =>
                        Logger[F].info(s"Task ${t.uuid} submitted, adding to list") *>
                        submittedJobInfo.update(_ + (jobId -> SubmittedJobInfo(device.platformId, t.circuit, submittedAt))) *>
                        submittedTasks.update(_ :+ (device.platform, device.platformId, jobId, t.uuid)) *> 
                        markDashboardSubmitted(
                            List(t.uuid),
                            provider = Some(device.platform),
                            deviceId = Some(device.platformId),
                            jobId = Some(jobId)
                        )
                    }
                    }
                  }    
                }
                cq <- submittedTasks.get
                _ <- Logger[F].info(s"Current Submitted: ${cq.mkString(", ")}")
            }yield ()


        private def startFetchingResults(): F[Unit] =  
            Stream
                .repeatEval(fetchAllInProgressJobResults)
                .metered(scala.concurrent.duration.FiniteDuration(100, "ms")) 
                .compile
                .drain
        
        def startRuntime: Resource[F, Unit] =
            SchedulerDashboard.resource[F](dashboardConfig, dashboardState) *>
            Resource
                .make {
                    (Concurrent[F].start(startScheduling), Concurrent[F].start(startFetchingResults)).tupled
                } { case (schedFib, fetchFib) =>
                    schedFib.cancel *> fetchFib.cancel
                }
                .void

        private def fetchAllInProgressJobResults(): F[Unit] =
            for {
                sts <- submittedTasks.get
                grouped = sts.groupBy { case (provider, _, jobId, _) => (provider, jobId) }

                _ <- grouped.toList.traverse_ {
                    case ((provider, jobId), entries) =>
                        fetchResultsFromCorrespondingProvider(
                            provider,
                            jobId,
                            entries
                        )
                }
                done <- completedTasks.get

                promotable <- pendingTasks.modify { ps =>
                val (goReady, stayPending) =
                    ps.partition(t => Scheduler.allParentResultsAvailable(done, t)) 
                (stayPending, goReady)
                }

                _ <- readyTasks.update(_ ++ promotable)
                _ <- markDashboardPendingReason(promotable.map(_.uuid), "ready")
            } yield ()
        
        //TODO On Failure of job this needs to reschedule 
        private def fetchResultsFromCorrespondingProvider(
            provider: String,
            providerId: String,
            entries: List[(String, String, String, TaskId)]
        ): F[Unit] =
            clients.providerClient(provider) match {
                case Some(providerClient) =>
                    providerClient.getTask(providerId).flatMap { status =>
                        providerClient.completedStatuses.contains(status.taskStatus) match {
                        case true  =>
                            persistSubmittedJobDataIfAvailable(providerClient, provider, providerId, entries, status) *>
                                fetchQuantumJobResult(providerClient, provider, providerId, entries, status).flatMap { result =>
                                    completeQuantumJob(provider, providerId, entries, result.summary, Some(result))
                                }
                        case false => Applicative[F].unit
                        }
                    }

                case None if provider == "Azure" =>
                    clients.azure.getQuantumTask(providerId).flatMap { r =>
                        r.status match {
                            case "Succeeded" => completeQuantumJob(provider, providerId, entries, "1", None)
                            case _           => Applicative[F].unit
                        }
                    }

                case None =>
                    new RuntimeException(s"No ProviderClient registered for platform=$provider").raiseError[F, Unit]
                }

        private def fetchQuantumJobResult(
            providerClient: ProviderClient[F],
            provider: String,
            providerId: String,
            entries: List[(String, String, String, TaskId)],
            status: ProviderTaskStatus
        ): F[QuantumJobResult] =
            providerClient.fetchTaskResult(providerId, status).attempt.flatMap {
                case Right(result) => result.pure[F]

                case Left(e) =>
                    val deviceId = entries.headOption.map(_._2)
                    Logger[F].warn(e)(s"Failed to fetch provider result for provider=$provider, job=$providerId") *>
                        QuantumJobResult.unavailable(
                            provider,
                            providerId,
                            deviceId,
                            s"Failed to fetch provider result: ${e.getMessage}"
                        ).pure[F]
            }

        private def persistSubmittedJobDataIfAvailable(
            providerClient: ProviderClient[F],
            provider: String,
            providerId: String,
            entries: List[(String, String, String, TaskId)],
            status: ProviderTaskStatus
        ): F[Unit] =
            if (!productionMode) Applicative[F].unit
            else
                providerClient.fetchJobTiming(providerId, status).attempt.flatMap {
                    case Left(e) =>
                        Logger[F].warn(e)(s"Failed to fetch provider timing for provider=$provider, job=$providerId")

                    case Right(ProviderJobTiming(Some(startedAt), Some(completedAt))) =>
                        submittedJobInfo.get.flatMap { submitted =>
                            submitted.get(providerId) match {
                                case Some(info) =>
                                    dataPersistanceService.persistSubmittedJobData(
                                        SubmittedJobDataCreate(
                                            jobId = providerId,
                                            provider = provider,
                                            deviceId = info.deviceId,
                                            submittedAt = info.submittedAt,
                                            startedAt = startedAt,
                                            completedAt = completedAt
                                        )
                                    )

                                case None =>
                                    val deviceId = entries.headOption.map(_._2).getOrElse("unknown")

                                    Logger[F].warn(
                                        s"Skipping submitted job data persistence for provider=$provider, device=$deviceId, job=$providerId; missing scheduler submit timestamp"
                                    )
                            }
                        }

                    case Right(_) =>
                        Logger[F].info(
                            s"Skipping submitted job data persistence for provider=$provider, job=$providerId; provider start/completion timestamps were incomplete"
                        )
                }

        private def submitJobWithFallback(device: Device, task: QuantumTask, candidates: List[CandidateDevice]): F[Unit] = {
            def bgAction(fa: F[Unit]): F[Unit] =
                fa.onError {
                    case _ =>
                    Logger[F].error(
                        s"Failed to submit job"
                    ) *>
                        Background[F].schedule(bgAction(fa), 1.hour)
                }

            val retryPolicy: RetryPolicy[F] =
                limitRetries[F](10) |+| exponentialBackoff[F](10.milliseconds)

            clients.providerClient(device.platform) match {
                case Some(providerClient) =>
                    val action = Retry[F]
                        .retry(retryPolicy)(
                            ProviderClient.submitQuantumTask(providerClient, device, task, task.circuit).void *>
                                Logger[F].info(s"Submitted task to ${providerClient.provider}")
                        )
                    bgAction(action)

                case None if device.platform == "Azure" =>
                    val action = Retry[F]
                        .retry(retryPolicy)(
                            clients.azure.submitJob(task.uuid.value.toString, task.toAzure) *>
                                Logger[F].info("Submitted task to Azure")
                        )
                    bgAction(action)

                case None =>
                    new RuntimeException(s"No ProviderClient registered for platform=${device.platform}").raiseError[F, Unit]
            }
        }
        
        private def unresolvedCompletionCost(
            taskId: TaskId,
            device: Device,
            visiting: Set[TaskId] = Set.empty
        ): F[Long] =
            if (visiting.contains(taskId)) 0L.pure[F]
            else {
                isCompletedTask(taskId).flatMap {
                    case true => 0L.pure[F]

                    case false =>
                        isSubmittedTask(taskId).flatMap {
                            case true => 0L.pure[F]

                            case false =>
                                lookupTask(taskId).flatMap {
                            case None =>
                                Logger[F].warn(s"Missing task metadata while estimating merge sync cost, taskId=$taskId") *>
                                0L.pure[F]

                            case Some(ct: ClassicalTask) =>
                                ct.parentTasks
                                    .traverse(pid => unresolvedCompletionCost(pid, device, visiting + taskId))
                                    .map(_.maxOption.getOrElse(0L))

                            case Some(qt: QuantumTask) =>
                                (
                                    qt.parentTasks
                                        .traverse(pid => unresolvedCompletionCost(pid, device, visiting + taskId))
                                        .map(_.maxOption.getOrElse(0L)),
                                    estimateRunTime(device, qt)
                                ).mapN(_ + _)

                            case Some(sgt: SyncronizedQuantumTaskList) =>
                                sgt.tasks
                                    .traverse(q => unresolvedCompletionCost(q.uuid, device, visiting + taskId))
                                    .map(_.maxOption.getOrElse(0L))
                                }
                        }
                }
            }

        private def estimateSynronizationCost(
            device: Device,
            tasks: List[QuantumTask]
        ): F[Long] =
            tasks.traverse { t =>
                t.parentTasks
                    .traverse(pid => unresolvedCompletionCost(pid, device))
                    .map(_.maxOption.getOrElse(0L))
            }.map { readyCosts =>
                if (readyCosts.isEmpty) 0L
                else readyCosts.max - readyCosts.min
            } 

        private def estimateRunTime(device: Device, task: QuantumTask): F[Long] =
            for {
                rawCal <- fetchDeviceCalibration(device, clients)
                cal = FidelityEstimator.normalizeCalibration(rawCal)

                totalGateDurationNs =
                task.circuit.remainingGates.foldLeft(0L) { (acc, gate) =>
                    acc + cal.durationNsFor(gate)
                }

                gateDurationMillis =
                math.ceil(totalGateDurationNs.toDouble / 1_000_000.0).toLong

                preparationMillis = 3000L
            } yield preparationMillis + gateDurationMillis

        
        private def gateCount(c: Circuit): Long =
            c.remainingGates.size.toLong 

        private def clamp(x: Double, lo: Double, hi: Double): Double = math.max(lo, math.min(hi, x))

        private def depthsWithinTolerance(
            group: List[QuantumTask],
            relTol: Double
        ): Boolean = {
            val depths = group.map(_.depth.value.toDouble).filter(_ > 0.0)

            depths match {
                case Nil => true
                case _ =>
                    val minDepth = depths.min
                    val maxDepth = depths.max
                    ((maxDepth - minDepth) / maxDepth) <= relTol
            }
        }

        private def canMergePendingGroup(
            group: List[QuantumTask],
            devices: List[Device]
        ): F[Boolean] = {
            val mergedQubits = group.map(_.qubits.value).sum
            val depthCompatible = depthsWithinTolerance(group, 0.30)

            if (group.size < 2 || mergedQubits > mergeMaxQubits || !depthCompatible) {
                false.pure[F]
            } else {
                val feasible = devices.filter(_.qubits >= mergedQubits)

                feasible
                    .traverse { d =>
                        estimateSynronizationCost(d, group).map { syncCost =>
                            syncCost < d.queueLength.toLong * mergeQueueFactorMillis
                        }
                    }
                    .map(_.exists(identity))
            }
        }

        private def createMergedPendingTask(group: List[QuantumTask]): F[QuantumTask] =
            for {
                mergedId <- ID.make[F, TaskId]
                logicalIds <- group
                    .traverse(q => logicalIdsFor(q.uuid))
                    .map(_.flatten.distinct)

                merged = QuantumTask(
                    uuid        = mergedId,
                    circuit     = mergeCircuits(group.map(_.circuit)),
                    qubits      = TaskQubits(group.map(_.qubits.value).sum),
                    shots       = TaskShots(group.map(_.shots.value).max),
                    depth       = TaskDepth(group.map(_.depth.value).max),
                    parentTasks = group.flatMap(_.parentTasks).distinct,
                    childTasks  = group.flatMap(_.childTasks).distinct,
                    createdAt   = group.map(_.createdAt).min
                )

                _ <- mergedAliases.update(_ + (mergedId -> logicalIds))
                _ <- rememberTask(merged)
            } yield merged

        private def growMergeGroup(
            seed: QuantumTask,
            rest: List[QuantumTask],
            devices: List[Device]
        ): F[(List[QuantumTask], List[QuantumTask])] =
            rest.foldLeftM((List(seed), List.empty[QuantumTask])) {
                case ((group, leftovers), cand) =>
                    val candidate = group :+ cand
                    val qsum = candidate.map(_.qubits.value).sum

                    if (qsum > mergeMaxQubits) {
                        (group, leftovers :+ cand).pure[F]
                    } else {
                        canMergePendingGroup(candidate, devices).map {
                            case true  => (candidate, leftovers)
                            case false => (group, leftovers :+ cand)
                        }
                    }
            }

        private def mergeQuantumRunGreedy(
            qs: List[QuantumTask],
            devices: List[Device]
        ): F[List[QuantumTask]] =
            qs match {
                case Nil => List.empty[QuantumTask].pure[F]

                case h :: t =>
                    growMergeGroup(h, t, devices).flatMap {
                        case (group, leftovers) =>
                            val currentF =
                                if (group.size >= 2) {
                                    createMergedPendingTask(group).map(List(_))
                                } else {
                                    List(h).pure[F]
                                }

                            currentF.flatMap(cur => mergeQuantumRunGreedy(leftovers, devices).map(cur ++ _))
                    }
            }

        private def mergePendingTasksPreservingOrder(
            tasks: List[Task],
            devices: List[Device]
        ): F[List[Task]] = {
            val quantumTasks = tasks.collect { case qt: QuantumTask => qt }
            val nonQuantumTasks = tasks.filter {
                case _: QuantumTask => false
                case _              => true
            }

            mergeQuantumRunGreedy(quantumTasks, devices).map { mergedQuantum =>
                mergedQuantum.map(identity[Task]) ++ nonQuantumTasks
            }
        }

        private def maybeMergePendingQuantumTasks: F[Unit] =
            if (!mergeEnabled) Applicative[F].unit
            else
                for {
                    devices  <- Scheduler.getAvailableDevices[F](clients)
                    before   <- pendingTasks.get
                    _ <- Logger[F].info(
                        s"[merge-debug] before pending = " +
                        before.collect {
                            case qt: QuantumTask =>
                                s"${qt.uuid.value}(q=${qt.qubits.value}, parents=${qt.parentTasks.map(_.value).mkString("[", ",", "]")})"
                            case other =>
                                s"${other.uuid.value}:${other.getClass.getSimpleName}"
                        }.mkString("[", ", ", "]")
                    )
                    after    <- mergePendingTasksPreservingOrder(before, devices)
                    _ <- Logger[F].info(
                        s"[merge-debug] after pending = " +
                        after.collect {
                            case qt: QuantumTask =>
                                s"${qt.uuid.value}(q=${qt.qubits.value}, parents=${qt.parentTasks.map(_.value).mkString("[", ",", "]")})"
                            case other =>
                                s"${other.uuid.value}:${other.getClass.getSimpleName}"
                        }.mkString("[", ", ", "]")
                    )
                    _        <- pendingTasks.set(after)
                    _        <-
                        if (after.size != before.size)
                            Logger[F].info(s"Pending merge pass: before=${before.size}, after=${after.size}")
                        else Applicative[F].unit
                } yield ()

        private def enqueuePendingQuantumTasksAndMaybeMerge(qs: List[QuantumTask]): F[Unit] =
            enqueuePending(qs.map(identity[Task])) *> maybeMergePendingQuantumTasks

        private def getAssignmentCoefficient(
            pTotal: Double,
            queueLength: Long,
            transpileMillis: Long,
            fleetMeanQueue: Double,
            observedQueueMillis: Option[Long] = None,
            observedFleetMeanQueueMillis: Option[Double] = None,
            lightLoadQueue: Double = 500.0,
            heavyLoadQueue: Double = 10000.0,
            lightLoadQueueMillis: Double = 1.minute.toMillis.toDouble,
            heavyLoadQueueMillis: Double = 1.hour.toMillis.toDouble,
            transpileScaleMillis: Double = 5000.0
        ): Double = {
            val load =
                observedFleetMeanQueueMillis match {
                    case Some(meanQueueMillis) =>
                        clamp(
                            (meanQueueMillis - lightLoadQueueMillis) / (heavyLoadQueueMillis - lightLoadQueueMillis),
                            0.0,
                            1.0
                        )

                    case None =>
                        clamp(
                            (fleetMeanQueue - lightLoadQueue) / (heavyLoadQueue - lightLoadQueue),
                            0.0,
                            1.0
                        )
                }

            val wF = 0.80 - 0.55 * load  //0.80 + 0.55  // - 0.75
            val wQ = 0.15 + 0.45 * load //0.15 - 0.45.  //0.75 + 0.75
            val wT = 1.0 - wF - wQ        

            val qScaled =
                observedQueueMillis match {
                    case Some(queueMillis) =>
                        clamp(
                            math.log1p(queueMillis.toDouble) / math.log1p(heavyLoadQueueMillis),
                            0.0,
                            1.0
                        )

                    case None =>
                        clamp(
                            math.log1p(queueLength.toDouble) / math.log1p(heavyLoadQueue),
                            0.0,
                            1.0
                        )
                }

            val tScaled =
                clamp(
                    transpileMillis.toDouble / transpileScaleMillis,
                    0.0,
                    1.0
                )

            wF * pTotal - wQ * qScaled - wT * tScaled
        }


        private def rememberTask(t: Task): F[Unit] =
            taskIndex.update(_ + (t.uuid -> t))

        private def rememberTasks(ts: List[Task]): F[Unit] =
            taskIndex.update(_ ++ ts.map(t => t.uuid -> t).toMap)

        private def logicalIdsFor(taskId: TaskId): F[List[TaskId]] =
            mergedAliases.get.map(_.getOrElse(taskId, List(taskId)))

        private def isSubmittedTask(taskId: TaskId): F[Boolean] =
            submittedTasks.get.map(_.exists { case (_, _, _, tid) => tid == taskId })

        private def isCompletedTask(taskId: TaskId): F[Boolean] =
            completedTasks.get.map(_.contains(taskId))

        private def lookupTask(taskId: TaskId): F[Option[Task]] =
            taskIndex.get.map(_.get(taskId))

            
        private def requiresCutting(task: NewQuantumTaskRequest, devices: List[Device]) : F[Boolean] = 
            devices
                .filter(_.qubits >= task.qubits.value)
                .traverse(d => Scheduler.estimateFidelity(d, task.circuit, clients, compiler))
                .map(lf => {
                    val x = lf.filter(_.pTotal > targetEstimatedFidelity)   //_.logPTotal > math.log(targetEstimatedFidelity))
                    println(s"========== HERE: ${math.log(targetEstimatedFidelity)} ========") 
                    println(x.mkString(", "))
                    x.isEmpty
                })

        ////////////////////////////////////////////////////////////
        private def allParentResultsAvailable(t: Task) : F[Boolean] = 
            completedTasks.get.map(done => Scheduler.allParentResultsAvailable(done, t))

        private def nowMillis: F[Long] =
            Clock[F].realTime.map(_.toMillis)

        private def nowUtcLocalDateTime: F[LocalDateTime] =
            Clock[F].realTimeInstant.map(LocalDateTime.ofInstant(_, ZoneOffset.UTC))

        private def submittedJobDataCutoff: F[LocalDateTime] =
            Clock[F].realTimeInstant.map { now =>
                LocalDateTime.ofInstant(
                    now.minusMillis(submittedJobDataLookback.toMillis),
                    ZoneOffset.UTC
                )
            }

        private def observedQueueMillisForDevice(device: Device): F[Option[Long]] =
            if (!productionMode) none[Long].pure[F]
            else
                for {
                    cutoff <- submittedJobDataCutoff
                    records <- dataPersistanceService.fetchSubmittedJobDataAfterDate(
                        date = cutoff,
                        provider = device.platform,
                        deviceId = device.platformId
                    )
                    waits = records.flatMap(_.queueWaitMillis)
                } yield waits match {
                    case Nil => None
                    case xs  => Some(xs.sum / xs.size)
                }

        private def observedQueueMillisByDevice(devices: List[Device]): F[Map[Device, Option[Long]]] =
            if (!productionMode) devices.map(_ -> none[Long]).toMap.pure[F]
            else devices.traverse(d => observedQueueMillisForDevice(d).map(d -> _)).map(_.toMap)

        private def markCompleted(
            taskId: TaskId,
            value: String,
            provider: Option[String] = None,
            deviceId: Option[String] = None,
            jobId: Option[String] = None,
            executedCircuit: Option[Circuit] = None,
            quantumResult: Option[QuantumJobResult] = None
        ): F[Unit] =
            nowMillis.flatMap { ts =>
                val completion = TaskCompletion(
                    taskId = taskId,
                    result = value,
                    completedAtMillis = ts,
                    provider = provider,
                    deviceId = deviceId,
                    jobId = jobId,
                    executedCircuit = executedCircuit,
                    quantumResult = quantumResult
                )

                val callbackF =
                    taskCallbacks.modify { callbacks =>
                        (callbacks - taskId, callbacks.get(taskId))
                    }.flatMap {
                        case Some(cb) =>
                            cb(completion).handleErrorWith { e =>
                                Logger[F].error(e)(s"Completion callback failed for task=${taskId.value}")
                            }
                        case None => Applicative[F].unit
                    }

                completedTasks.update(_ + taskId) *>
                taskIndex.update(_ - taskId) *>
                dashboardState.update(st =>
                    SchedulerDashboard.markCompleted(st, completion, dashboardConfig.retainedCompletedTasks)
                ) *>
                callbackF
            }

        private def completeQuantumJob(
            provider: String,
            jobId: String,
            entries: List[(String, String, String, TaskId)],
            resultValue: String,
            quantumResult: Option[QuantumJobResult]
        ): F[Unit] =
            for {
                jobInfoMap <- submittedJobInfo.get
                executedCircuit = jobInfoMap.get(jobId).map(_.executedCircuit)
                deviceId = entries.headOption.map(_._2)
                taskIds = entries.map(_._4).distinct
                _ <- taskIds.traverse_ { tid =>
                    markCompleted(
                        taskId = tid,
                        value = resultValue,
                        provider = Some(provider),
                        deviceId = deviceId,
                        jobId = Some(jobId),
                        executedCircuit = executedCircuit,
                        quantumResult = quantumResult
                    )
                }
                _ <- submittedTasks.update(_.filterNot { case (p, _, jid, _) => p == provider && jid == jobId })
                _ <- submittedJobInfo.update(_ - jobId)
            } yield ()

        def estimateQueueTime(device: Device, task: QuantumTask) : F[Long] = {
            val windowSize = 14L
            val windowHours = 2L
            val recencyHalfLifeDays : Double = 3
            val zoneId: ZoneId = ZoneId.systemDefault()
            val gaussianKernel = false 
            val timeKernelSigmaMinutes = 60.0

            def minuteDiff(a: Int, b: Int): Int = {
                val diff = abs(a - b)
                min(diff, 1440 - diff)
            }

            def recencyWeight(sampleInstant: Instant, now: Instant): Double = {
                val ageMinutes = java.time.Duration.between(sampleInstant, now).toMinutes.toDouble.max(0.0)
                exp(-log(2.0) * ageMinutes / (recencyHalfLifeDays * 24.0 * 60.0))
            }

            def timeOfDayWeight(deltaMinutes: Int): Double =
                if (!gaussianKernel) {
                    if (deltaMinutes <= (windowHours * 60).toInt) 1.0 else 0.0
                } else {
                    val sigma2 = timeKernelSigmaMinutes * timeKernelSigmaMinutes
                    exp(-(deltaMinutes.toDouble * deltaMinutes.toDouble) / (2.0 * sigma2))
                }

            def weightedMean(values: List[(Long, Double)]): Option[Long] = {
                val denom = values.map(_._2).sum
                if (denom <= 0.0) None
                else {
                    val num = values.map { case (v, w) => v.toDouble * w }.sum
                    Some(math.round(num / denom))
                }
            }

            def meanField(f: DeviceQueueInformation => Option[Int], weighted: List[(DeviceQueueInformation, Double)]): Option[Long] = {
                val pairs = weighted.flatMap { case (x, w) => f(x).map(v => (v.toLong, w)) }
                weightedMean(pairs)
            } //not using for now 


            for{
                now <- Clock[F].realTimeInstant
                nowZdt      = now.atZone(zoneId)
                cutoffLdt   = nowZdt.minusDays(windowSize).toLocalDateTime
                historicData <- dataPersistanceService.fetchQueueInformationAfterDate(cutoffLdt, device.platformId)
                nowMinuteOfDay  = nowZdt.getHour * 60 + nowZdt.getMinute
                maxDeltaMinutes = (windowHours * 60).toInt
                weighted  = historicData.flatMap { x =>
                    val xi = x.createdAt.atZone(zoneId).toInstant
                    val minuteOfDay = x.createdAt.getHour * 60 + x.createdAt.getMinute
                    val dt = minuteDiff(minuteOfDay, nowMinuteOfDay)

                    val w = recencyWeight(xi, now) * timeOfDayWeight(dt)
                    if (w <= 0.0) None else Some((x, w))
                }

                queueMean =
                    weightedMean(weighted.map { case (x, w) => (x.queueLength.toLong, w) })
                    .getOrElse(0L)
            }yield queueMean
        }

        // not used
        private def estimateTranspilationTime(circuit: Circuit, targetGateSet: List[Gate]) : F[Long] = 
            (circuit.remainingGates.length / 1000000L).pure[F] 
            // This is very dumb and will likely get removed. 
            // We need to transpile to decide on other factors anyway so accounting for potential transpilation not needed

        private def enqueueReady(newTasks: List[Task]): F[Unit] =
            readyTasks.update(ts => prioritizationStrategy(newTasks ++ ts))

        private def enqueuePending(newTasks: List[Task]): F[Unit] =
            pendingTasks.update(ts => newTasks ++ ts)

        private def fakeClassicalTaskScheduler(ct: ClassicalTask): F[Unit] = {
            val computationTime = 500 + Random.nextInt(1500)
            markDashboardSubmitted(
                List(ct.uuid),
                provider = None,
                deviceId = None,
                jobId = None,
                note = Some("Classical execution")
            ) *>
            Temporal[F].sleep(computationTime.millis) *>
            markCompleted(ct.uuid, s"Result of classical task ${ct.uuid.value}")
        }

        def getSubmittedTasks(): F[List[(String, String, String, TaskId)]] = 
            submittedTasks.get
    }


    private[qurator] def allParentResultsAvailable(
        completedTasks: Set[TaskId],
        t: Task
    ): Boolean =
        t match {
            case ct: ClassicalTask =>
                ct.parentTasks.forall(pid => completedTasks.contains(pid))

            case qt: QuantumTask =>
                qt.parentTasks.forall(pid => completedTasks.contains(pid))

            case sgt: SyncronizedQuantumTaskList =>
                sgt.tasks.forall(child => allParentResultsAvailable(completedTasks, child))
        }

    
        //single pass, greedy 
        private[qurator] def bucketByDepth(tasks: List[QuantumTask], depthRelTol: Double): List[List[QuantumTask]] = { 
            final case class Bucket(tasks: List[QuantumTask], meanDepth: Double) {
                def size: Int = tasks.size
            }

            def withinMeanBound (mean: Double, d: Int): Boolean = {
                val md = math.max(mean, 1.0)
                (math.abs(d.toDouble - mean) / md) <= depthRelTol
            }

            val sorted = tasks.sortBy(_.depth.value)

            val buckets: List[Bucket] =
                sorted.foldLeft(List.empty[Bucket]) { (acc, t) =>
                    acc match {
                        case Nil =>
                            Bucket(List(t), t.depth.value.toDouble) :: Nil

                        case b :: rest =>
                            val d = t.depth.value
                            if (withinMeanBound(b.meanDepth, d)) {
                                val newTasks = t :: b.tasks
                                val newMean =
                                (b.meanDepth * b.tasks.size.toDouble + d.toDouble) / newTasks.size.toDouble
                                Bucket(newTasks, newMean) :: rest
                            } else {
                                Bucket(List(t), d.toDouble) :: acc
                            }
                    }
                }

            buckets.reverse.map(b => b.tasks.reverse)
        }

        private[qurator] def attemptToMergeSyncTasks[F[_] : MonadThrow : GenUUID : Logger](
            tasks: List[QuantumTask],
            clients: HttpClients[F],
            compiler: FakeCompiler[F],
            targetEstimatedFidelity: Double
        ): F[List[QuantumTask]] = 
            for{
                devices <- getAvailableDevices[F](clients)
                maxQubits = devices.map(_.qubits).maxOption.getOrElse(0)
                depthTolerance = 0.20 
                depthBuckets = Scheduler.bucketByDepth(tasks, depthTolerance)
                groups = depthBuckets.flatMap { bucket =>
                    assignToFinalBuckets(
                        bucket = bucket,
                        capacity = maxQubits, 
                        maxTasksPerBin = 3 //fix this, obv shouldn't be constant
                    )
                }
                merged <- groups.traverse(g => Scheduler.flattenGroup(g, devices, clients, compiler, targetEstimatedFidelity))
            } yield merged.flatten
        
        private[qurator] def getAvailableDevices[F[_]: MonadThrow](clients: HttpClients[F]): F[List[Device]] =
            for {
                providerDevices <- clients.providerClients.traverse(_.fetchAvailableDevices.attempt)
                azureE  <- clients.azure.fetchDeviceInformation.attempt

                quantumDevices = providerDevices.flatMap(_.toOption.getOrElse(Nil))

                azure = azureE.toOption.toList.flatMap(_.value)
                .filter(_.currentAvailability == "Available")
                .map(_.toDevice)
            } yield quantumDevices ++ azure

        private[qurator] def assignToFinalBuckets( 
            bucket: List[QuantumTask],
            capacity: Int,
            maxTasksPerBin: Int
        ): List[List[QuantumTask]] = { 
            final case class Bin(tasks: List[QuantumTask], used: Int) {
                def canFit(t: QuantumTask): Boolean =
                    (used + t.qubits.value <= capacity) && (tasks.size < maxTasksPerBin)
                def add(t: QuantumTask): Bin = Bin(tasks :+ t, used + t.qubits.value)
            }

            val sorted = bucket.sortBy(t => -t.qubits.value)

            val bins = sorted.foldLeft(List.empty[Bin]) { (binsAcc, t) =>
                val idx = binsAcc.indexWhere(_.canFit(t))
                if (idx >= 0) {
                    binsAcc.updated(idx, binsAcc(idx).add(t))
                } else {
                    Bin(List(t), t.qubits.value) :: binsAcc
                }
            }

            bins.reverse.map(_.tasks)
        }


        private[qurator] def flattenGroup[F[_]: MonadThrow : GenUUID : Logger](
            group: List[QuantumTask], 
            devices: List[Device],
            clients: HttpClients[F],
            compiler: FakeCompiler[F],
            targetEstimatedFidelity: Double
        ): F[List[QuantumTask]] = {
            println("Starting Flatten Group")
            println(s"Group Size ${group.length}")
            group match {
                case Nil          => List.empty[QuantumTask].pure[F]
                case single :: Nil => List(single).pure[F]
                case g =>
                    val mergedQubits = g.map(_.qubits.value).sum
                    val feasibleDevices = devices.filter(_.qubits >= mergedQubits)

                    if (feasibleDevices.isEmpty) {
                        g.pure[F]
                    } else {
                        val mergedCircuit: Circuit =
                            mergeCircuits(g.map(_.circuit)) 
                        ID.make[F, TaskId].flatMap { mergedId =>
                            val mergedTask =
                                QuantumTask(
                                    uuid        = mergedId,
                                    circuit     = mergedCircuit,
                                    qubits      = TaskQubits(mergedQubits),
                                    shots       = TaskShots(g.map(_.shots.value).max),  
                                    depth       = TaskDepth(g.map(_.depth.value).max),   
                                    parentTasks = g.flatMap(_.parentTasks).distinct,    
                                    childTasks  = g.map(_.uuid),                         
                                    createdAt   = g.map(_.createdAt).min
                                )

                            feasibleDevices
                                .traverse(d => Scheduler.estimateFidelity(d, mergedTask.circuit, clients, compiler)) 
                                .map(_.map(_.logPTotal).maxOption.getOrElse(0.0))
                                .flatMap { bestFidelity =>
                                    Logger[F].info(s"Group Size: ${group.size}, mergedQubits: ${mergedQubits}, feasibleDevices: ${feasibleDevices.size}, bestFidelity: ${bestFidelity}, targetFidelity: ${math.log(targetEstimatedFidelity)}") *>
                                    {if (bestFidelity >= math.log(targetEstimatedFidelity)) List(mergedTask).pure[F]
                                    else g.pure[F]}
                                }
                            }
                    }
            } 
        }

            private def mergeCircuits(circuits: List[Circuit]): Circuit = circuits.foldLeft(Circuit(List.empty[Gate], 0)){(acc, b) => {
                val offset = acc.qubits
                val shiftedGates = b.remainingGates.map{
                    case X(q) => X(q + offset)
                    case H(ctrl) => H(ctrl + offset)
                    case CX(ctrl, target) => CX(ctrl + offset, target + offset)
                    case CCX(ctrl1, ctrl2, target) => CCX(ctrl1 + offset, ctrl2 + offset, target + offset)
                    case CZ(ctrl, target) => CZ(ctrl + offset, target + offset)
                    case U(theta, phi, lambda, q) => U(theta, phi, lambda, q + offset)
                    case CU(ctrl, theta, phi, lambda, target) => CU(ctrl + offset, theta, phi, lambda, target + offset)
                    case Swap(q1, q2) => Swap(q1 + offset, q2 + offset)
                    case CRZ(ctrl, thetaDenom, q) => CRZ(ctrl + offset, thetaDenom, q + offset)
                    case RZ(thetaDenom, q) => RZ(thetaDenom, q + offset)
                    case RY(theta, q) => RY(theta, q + offset)
                    case RX(theta, q) => RX(theta, q + offset)
                    case SX(q) => SX(q + offset)
                    case Measure(q) => Measure(q + offset)
                }
                Circuit(acc.remainingGates ++ shiftedGates, acc.qubits + b.qubits) //TODO Update this to merge gates based on slices 
            }}  

            private[qurator] def estimateFidelity[F[_]: MonadThrow](
                device: Device, 
                task: Circuit, 
                clients: HttpClients[F],
                compiler: FakeCompiler[F]) : F[FidelityEstimate] =  
                for{
                    compiled <- compiler.compileCircuitFor(device, task)
                    deviceCal <- Scheduler.fetchDeviceCalibration(device, clients) 
                    cal = FidelityEstimator.normalizeCalibration(deviceCal)
                    est = FidelityEstimator.score(compiled, cal)
                } yield est

            // Circuit Depth, Avg. CX error over the circuit, Avg CX in the circuit critical path, readout errors on the measured qubits. 
            //The model is built as a product of linear terms: Fn =
            //Π(ai + bi ∗ xi), where Fn is the fidelity of job n, xi is the feature and ai and bi are the tuned coefficient ??

            private def fetchDeviceCalibration[F[_]: MonadThrow](device: Device, clients: HttpClients[F]): F[DeviceCalibration] =
                clients.providerClient(device.platform).map(_.fetchDeviceCalibration(device.platformId)).getOrElse {
                    if (device.platform == "Azure")
                        clients.azure.fetchDeviceCalibration(device.platformId)
                    else
                        new RuntimeException(s"No ProviderClient registered for platform=${device.platform}")
                            .raiseError[F, DeviceCalibration]
                }

            private[qurator] def buildGreedySynchronizedPlan[F[_]: Monad : Logger : MonadCancelThrow](
                orderedTasks: List[QuantumTask],
                candidatesByTask: Map[QuantumTask, List[CandidateDevice]],
                t1BudgetMillis: Long
            ): F[SynchronizedPlan] = {

                final case class Placement(
                    task: QuantumTask,
                    device: Device,
                    startMillis: Long,
                    finishMillis: Long
                )

                final case class Acc(
                    assignments: Map[Device, List[QuantumTask]],
                    runtimeSum: Map[Device, Long],
                    placements: List[Placement]
                )

                def taskStartFinish(device: Device, cand: CandidateDevice, acc: Acc): (Long, Long) = {
                    val prevRuntimeOnDevice = acc.runtimeSum.getOrElse(device, 0L)
                    val start = cand.queueMillis + prevRuntimeOnDevice
                    val finish = start + cand.runMillis
                    (start, finish)
                }

                def objective(starts: List[Long], finishes: List[Long]): Long = {
                    val spreadStart =
                        starts.maxOption.getOrElse(0L) - starts.minOption.getOrElse(0L)

                    val spreadFinish =
                        finishes.maxOption.getOrElse(0L) - finishes.minOption.getOrElse(0L)

                    val t1Penalty =
                        if (t1BudgetMillis > 0L && spreadFinish > t1BudgetMillis)
                            (spreadFinish - t1BudgetMillis) * 10L
                        else 0L

                    spreadStart + (spreadFinish / 2L) + t1Penalty
                }

                def chooseBestDeviceForTask(t: QuantumTask, acc: Acc): F[(Device, CandidateDevice, Long, Long)] = {
                    val candidates = candidatesByTask.getOrElse(t, Nil)

                    if (candidates.isEmpty) {
                        Logger[F].warn(s"No candidates computed for task=${t.uuid}") *>
                        (new RuntimeException("No candidates for task"))
                            .raiseError[F, (Device, CandidateDevice, Long, Long)]
                    } else {
                        val currentStarts   = acc.placements.map(_.startMillis)
                        val currentFinishes = acc.placements.map(_.finishMillis)

                        candidates
                            .traverse { cand =>
                                val (start, finish) = taskStartFinish(cand.device, cand, acc)

                                val starts2   = start :: currentStarts
                                val finishes2 = finish :: currentFinishes

                                val obj = objective(starts2, finishes2)

                                (obj, -cand.fidelity, cand.queueMillis, cand, start, finish).pure[F]
                            }
                            .map { scored =>
                                val (_, _, _, bestCand, bestStart, bestFinish) =
                                    scored.minBy { case (obj, negFid, q, _, _, _) => (obj, negFid, q) }

                                (bestCand.device, bestCand, bestStart, bestFinish)
                            }
                    }
                }

                orderedTasks
                    .foldLeftM(
                        Acc(
                            assignments = Map.empty[Device, List[QuantumTask]],
                            runtimeSum = Map.empty[Device, Long],
                            placements = Nil
                        )
                    ) { (acc, t) =>
                        chooseBestDeviceForTask(t, acc).map { case (d, cand, start, finish) =>
                            val updatedAssignments =
                                acc.assignments.updated(d, acc.assignments.getOrElse(d, Nil) :+ t)

                            val updatedRuntimeSum =
                                acc.runtimeSum.updated(d, acc.runtimeSum.getOrElse(d, 0L) + cand.runMillis)

                            val updatedPlacements =
                                Placement(t, d, start, finish) :: acc.placements

                            Acc(
                                assignments = updatedAssignments,
                                runtimeSum = updatedRuntimeSum,
                                placements = updatedPlacements
                            )
                        }
                    }
                    .map(acc => SynchronizedPlan(acc.assignments))
            }
}
