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

trait Scheduler[F[_]]{
    def submitTask(taskReq: TaskRequest): F[List[TaskId]]
    def estimateQueueTime(device: Device, task: QuantumTask) : F[Long]
    def getSubmittedTasks(): F[List[(String, String, String, TaskId)]]
    def getResults(): F[Map[TaskId, (String, Long)]]
    def startRuntime: Resource[F, Unit]
}

object Scheduler{
  def make[F[_]: GenUUID: Concurrent: Logger : Temporal : Background](
        dataPersistanceService: DataPersistanceService[F],
        clients: HttpClients[F],
        prioritizationStrategy: List[Task] => List[Task],
        cuttingStrategy: (Circuit, List[Device]) => F[List[Circuit]],
        targetEstimatedFidelity: Double, 
        additionalOptimizationRuns: Circuit => List[Circuit],
        compiler: FakeCompiler[F] //abstract this 
  ): F[Scheduler[F]] =
    for {
      readyTasks     <- Ref.of[F, List[Task]](List.empty)
      pendingTasks   <- Ref.of[F, List[Task]](List.empty)
      submittedTasks <- Ref.of[F, List[(String, String, String, TaskId)]](List.empty) //platform, platformId, jobId, taskId
      results        <- Ref.of[F, Map[TaskId, (String, Long)]](Map.empty)    
      _ <- Logger[F].info("Creating The Scheduler")    
    } yield new Scheduler[F] {

        private val idleDelay: FiniteDuration = 250.millis

       
        //TODO Merge Cut Task Results 
        //TODO for synronized tasks, can cutting be done more intelligently to isolate non-entangled parts?
        //TODO consider impact of cross talk when scheduling multiple tasks on the same device --> need topology aware mapping. Defined as avg distance between data qubits 
        //TODO Use estimateSynronizationCost to implement merging. Downside, this requires time estimation for classical tasks.
        //TODO: Estimate preperation time and add to queue time (and use entanglement estimation for runtime estimation)
        //TODO: Loop back actual job data 

        /////////////////////////////////////////////// NOT ADDRESSING NOW ////////////////////////////////////////////
        //TODO There is a possibility that merging tasks early limits the devices in the syncronization stage later on. 
        //TODO Fall back to other devices on failure (maybe after expontential backoff ?)
        //TODO think about reservations?? 
        //TODO add Result types (we can do this after the paper is done, dummy results for the sake of evaluation is fine for now) 
        //TODO: I think buildGreedySynchronizedPlan needs to be revised (chain scheduling issue)
        //TODO: We need to move some of the logic to supervisor so that the scheduler keeps running on error 
        //TODO: Stronger topology mapping 


        def submitTask(taskReq: TaskRequest): F[List[TaskId]] = taskReq match{
            case str : SynronizedQuantumTaskRequest => 
                Logger[F].info("Received Synronized Task") *> submitSynronizedTaskRequest(str)
            case ntr : NewQuantumTaskRequest => 
                 Logger[F].info("Received Quantum Task") *>  submitNewTaskRequest(ntr)
            case ctr : NewClassicalTaskRequest => 
                 Logger[F].info("Received Classical Task") *>  submitClassicalTaskRequest(ctr)
        }

        private def submitClassicalTaskRequest(taskReq: NewClassicalTaskRequest): F[List[TaskId]] = 
            ID.make[F, TaskId].flatMap(taskId => {
                val t = ClassicalTask( 
                        uuid = taskId,
                        program = taskReq.program,
                        parentTasks = taskReq.parentTasks,
                        childTasks = taskReq.childTasks,
                        createdAt = taskReq.createdAt
                )
                allParentResultsAvailable(t).flatMap{ apr =>
                    if(taskReq.parentTasks.isEmpty || apr){
                        enqueueReady(List(t)) *> List(t.uuid).pure[F]

                    }else{
                        enqueuePending(List(t)) *> List(t.uuid).pure[F]
                    }
                }
            })

        private def submitNewTaskRequest(taskReq: NewQuantumTaskRequest): F[List[TaskId]] =  // TST
             for{
                devices <- Scheduler.getAvailableDevices[F](clients)
                needsToBeCut <- requiresCutting(taskReq, devices)
                _ <- Logger[F].info(s"Processing New Quantum Task, it requries cutting?: $needsToBeCut")
                tids <- 
                    if(needsToBeCut){ //TODO: Think this through carefully. I am not convinced this is the right place to cut. 
                        for {
                            cut <- cuttingStrategy(taskReq.circuit, devices)
                            optimized = cut.flatMap(additionalOptimizationRuns(_))
                            recreatedTasks <- optimized.traverse { c =>
                                ID.make[F, TaskId].map { taskId =>
                                    QuantumTask(
                                        taskId,
                                        c,
                                        taskReq.qubits,
                                        taskReq.shots,
                                        taskReq.depth,
                                        taskReq.parentTasks,
                                        taskReq.childTasks,
                                        taskReq.createdAt
                                    )
                                }
                            }
                            tids <-
                                if (taskReq.parentTasks.nonEmpty) enqueuePending(recreatedTasks) *> List(recreatedTasks.map(_.uuid): _*).pure[F]
                                else enqueueReady(recreatedTasks) *> List(recreatedTasks.map(_.uuid): _*).pure[F]
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
                            if(taskReq.parentTasks.nonEmpty){
                                enqueuePending(List(t)) *> 
                                 (readyTasks.get, pendingTasks.get).tupled.flatMap { case (r, p) =>
                                        Logger[F].info(s"enqueuePending: ready=${r.size}, pending=${p.size}") } *>
                                List(t.uuid).pure[F]
                            } else {
                                enqueueReady(List(t)) *>  
                                (readyTasks.get, pendingTasks.get).tupled.flatMap { case (r, p) =>
                                    Logger[F].info(s"enqueuePending: ready=${r.size}, pending=${p.size}") } *>
                                 List(t.uuid).pure[F]}
                        })
                    }
            } yield tids 

        private def submitSynronizedTaskRequest(str: SynronizedQuantumTaskRequest): F[List[TaskId]] =  // TST
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
                _ <- Logger[F].info(s"Attempting Merge Sync Tasks")
                possiblyMergedTasks <- Scheduler.attemptToMergeSyncTasks(tasks, clients, compiler, targetEstimatedFidelity) 
                _ <- Logger[F].info(
                    s"Merge attempt: original=${tasks.size}, after=${possiblyMergedTasks.size}, " +
                    s"ids=${possiblyMergedTasks.map(_.uuid).mkString(", ")}"
                )
                groupId <- ID.make[F, TaskId]
                sg = SyncronizedQuantumTaskList(
                    groupId,
                    possiblyMergedTasks,
                    str.t1Budget,
                    LocalDateTime.now()
                )
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

       private def scheduleOneQuantumTask(task: QuantumTask): F[Unit] =
            for {
                devices <- Scheduler.getAvailableDevices(clients)

                _ <- Logger[F].info(s"Attempting to schedule quantum task ${task.uuid}")
                _ <- Logger[F].info(s"Devices ${devices.map(d => d.platformId)}")

                suitableDevices = devices.filter(d => d.qubits >= task.qubits.value)

                _ <- suitableDevices match {
                case Nil =>
                    Logger[F].warn(s"No eligible devices for task=${task.uuid}; re-enqueueing") //TODO: re-queue here for production ready ver. 

                case ds =>
                    for {
                        scored <- ds.traverse { d =>
                            (estimateFidelity(d, task.circuit, clients, compiler),
                                estimateTranspilationTime(task.circuit, d.gateSet)
                            ).mapN { (f, tMillis) =>
                                val ac = getAssignmentCoefficient(f.logPTotal, f.pTotal, d.queueLength + tMillis, task.circuit)
                                (d, ac, f, d.queueLength)
                            }
                        }

                        best = scored.maxBy { case (_, ac, f, q) => (ac, f.logPTotal, -q) }
                        _ <- Logger[F].info(s"Picked Device Coefficient: $best")
                        (bestDevice, _, _, _) = best

                        // compiled <- ???

                        jobId <- submitQuantumToProvider(bestDevice, task, task.circuit)
                        _ <- submittedTasks.update(_ :+ (bestDevice.platform, bestDevice.platformId, jobId, task.uuid))
                    } yield ()
                }
            } yield ()

        private def submitQuantumToProvider(
            device: Device,
            task: QuantumTask,
            compiled: Circuit
        ): F[String] =
        device.platform match {

            case "Braket" =>
                for {
                    token <- UUID.randomUUID().toString.pure[F] //not needed for tests, will wire later
                    qasmSource = "" //not needed for tests, will wire later
                    req = BraketCreateQuantumTaskRequest(
                        action = "braket.ir.openqasm.program",
                        associations = None,
                        clientToken = token,
                        deviceArn = device.platformId,
                        deviceParameters = "{}",
                        shots = task.shots.value
                    )
                    _ <- Logger[F].info(s"Submitting task ${task.uuid} to device $device") 
                    resp <- clients.braket.submitBraketOpenQasmTask(req, qasmSource)
                } yield resp.quantumTaskArn

            case "IBM" => // To Fix
                val req = 
                     SubmitJobRequestV2(
                        "sampler",
                        device.platformId,
                        None,
                        None,
                        Some("info"),
                        None,
                        None,
                        None,
                        SamplerV2Input(
                            pubs = List(
                                "" //transform to qasm here
                            )
                        )
                    )
                Logger[F].info(s"Submitting task ${task.uuid} to device $device") *> clients.ibm.submitJob(req).map(_.id)

            case "Azure" => //Azure is not playing by the rules so we will deal with them later 
                new RuntimeException("Azure submit not wired in scheduleOneQuantumTask yet").raiseError[F, String]

            case other =>
                new RuntimeException(s"Unknown platform=$other").raiseError[F, String]
        }


        private def scheduleSynronizedTasks(s: SyncronizedQuantumTaskList): F[Unit] =
            for{
                _ <- Logger[F].info("Scheduling Syncronized Task")
                devices <- Scheduler.getAvailableDevices(clients)
                orderedTasks = prioritizationStrategy(s.tasks).collect{ case t: QuantumTask => t}
                candidateDevicesByTask <- orderedTasks.traverse{t =>
                    val suitableDevices = devices.filter(d => d.qubits >= t.qubits.value)
                    suitableDevices.traverse{d => 
                        (estimateFidelity(d, t.circuit, clients, compiler), estimateTranspilationTime(t.circuit, d.gateSet), estimateRunTime(d,t))
                            .mapN{(f, t, run) => 
                               val queueMillis = d.queueLength + t
                               CandidateDevice(d, fidelity = f.logPTotal, queueMillis = queueMillis, runMillis = run)
                            }
                    }.map{cs => 
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
                    submitQuantumToProvider(device, t, t.circuit).flatMap(jobId => 
                        Logger[F].info(s"Task ${t.uuid} submitted, adding to list") *>
                        submittedTasks.update(_ :+ (device.platform, device.platformId, jobId, t.uuid))
                    )
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
                _   <- sts.traverse_ { case (provider, providerId, jobId, taskId) =>
                        fetchResultsFromCorrespondingProvider(provider, jobId, taskId)
                    }
                rs <- results.get
                //_ <- submittedTasks.update(_.filterNot { case (_, _, _, tid) => rs.contains(tid) }) //TODO: Uncomment this later when you find a fix for benchmark suite

                promotable <- pendingTasks.modify { ps =>
                val (goReady, stayPending) =
                    ps.partition(t => Scheduler.allParentResultsAvailable(rs, t)) 
                (stayPending, goReady)
                }

                _ <- readyTasks.update(_ ++ promotable)
            } yield ()
        
        //TODO: On Failure of job this needs to reschedule 
        private def fetchResultsFromCorrespondingProvider(provider: String, providerId: String, taskId: TaskId): F[Unit] = provider match{
            case "IBM" => 
                clients.ibm.listJobDetails(providerId).flatMap { r =>
                    r.status match {
                    case "Completed" => markCompleted(taskId, "1")
                    case _           => Applicative[F].unit
                    }
                }
            case "Braket" =>
                clients.braket.getQuantumTask(providerId).flatMap { r =>
                    r.status match {
                    case "COMPLETED" => markCompleted(taskId, "1")
                    case _           => Applicative[F].unit
                    }
                }
            case "Azure" => 
                clients.azure.getQuantumTask(providerId).flatMap { r =>
                    r.status match {
                        case "Succeeded" => markCompleted(taskId, "1")
                        case _           => Applicative[F].unit
                    }
                }
        } 

        //TODO: Once we unify the client traits, all this pattern matching will go away 
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

            device.platform match {
                case "IBM" => 
                    val action = Retry[F]
                        .retry(retryPolicy)(clients.ibm.submitJob(task.toIBM) *> Logger[F].info("Submitted Task to IBM"))
                        // .adaptError {
                        //     case e => () //TODO: fallback to another device here 
                        // }
                    bgAction(action)
                case "Braket" => 
                    val action = Retry[F]
                        .retry(retryPolicy)(clients.braket.submitBraketOpenQasmTask(task.toBraket, task.circuit.toQasm) *> Logger[F].info("Submitted Task to IBM"))
                        // .adaptError {
                        //     case e => ()
                        // }
                    bgAction(action)
                case "Azure" => 
                    val action = Retry[F]
                        .retry(retryPolicy)(clients.azure.submitJob(task.uuid.value.toString, task.toAzure) *> Logger[F].info("Submitted Task to IBM"))
                        // .adaptError {
                        //     case e => ()
                        // }
                    bgAction(action)
            }
        }
        
        private def estimateSynronizationCost(tasks: List[QuantumTask]): F[Long] = 0L.pure[F] 

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

        private def getAssignmentCoefficient(
            logFidelity: Double,        
            pTotal: Double,
            queueLength: Long,          
            circuit: Circuit,
            transpileCostGates: Long = 0,    
            lambda: Double = 1e-4       
        ): Double = {
            val g = math.max(1L, gateCount(circuit))                 
            val latencyUnits = queueLength.toDouble * g.toDouble + transpileCostGates.toDouble
            val qNorm = math.log1p(latencyUnits.toDouble)  
            val wF    = 1.0 / (1.0 + qNorm)                     
            val wQ    = 1.0 - wF
            //logFidelity - lambda * latencyUnits
            wF * pTotal - wQ * qNorm
        }

    
        private def requiresCutting(task: NewQuantumTaskRequest, devices: List[Device]) : F[Boolean] = 
            devices.traverse(d => Scheduler.estimateFidelity(d, task.circuit, clients, compiler)).map(lf => lf.filter(_.logPTotal > math.log(targetEstimatedFidelity)).nonEmpty)

        ////////////////////////////////////////////////////////////
        private def allParentResultsAvailable(t: Task) : F[Boolean] = 
            results.get.map(rs => Scheduler.allParentResultsAvailable(rs, t))

        private def nowMillis: F[Long] =
            Clock[F].realTime.map(_.toMillis)

        private def markCompleted(taskId: TaskId, value: String): F[Unit] =
            nowMillis.flatMap { ts =>
                results.update(_.updated(taskId, (value, ts)))
            }

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
            Temporal[F].sleep(computationTime.millis) *> 
            Logger[F].info(s"Ran Classical Task, tid: ${ct.uuid}") *>
            markCompleted(ct.uuid, s"Result of classical task ${ct.uuid.value}")
        }

        def getSubmittedTasks(): F[List[(String, String, String, TaskId)]] = 
            submittedTasks.get

        def getResults() = 
            results.get
    }


    private[qurator] def allParentResultsAvailable(
        results: Map[TaskId, (String, Long)],
        t: Task
    ): Boolean =
        t match {
            case ct: ClassicalTask =>
                ct.parentTasks.forall(pid => results.get(pid).nonEmpty)

            case qt: QuantumTask =>
                qt.parentTasks.forall(pid => results.get(pid).nonEmpty)

            case sgt: SyncronizedQuantumTaskList =>
                sgt.tasks.forall(child => allParentResultsAvailable(results, child))
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
                ibmE    <- clients.ibm.fetchDeviceInformation.attempt
                braketE <- clients.braket.fetchDeviceList.attempt
                azureE  <- clients.azure.fetchDeviceInformation.attempt

                ibm = ibmE.toOption.toList.flatMap(_.devices)
                .filter(_.status.name == "online")
                .map(_.toDevice)

                braketOnline = braketE.toOption.toList.flatMap(_.devices)
                .filter(d => d.deviceStatus == "ONLINE" && deviceActive(d))

                braketDetails <- clients.braket.fetchDeviceDetails(braketOnline.map(_.deviceArn))
                braket = braketDetails.map(_.toDevice)

                _ = println(s"Braket Devices ${braket.mkString(", ")}")

                azure = azureE.toOption.toList.flatMap(_.value)
                .filter(_.currentAvailability == "Available")
                .map(_.toDevice)
            } yield ibm ++ braket ++ azure

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


        private[qurator] def flattenGroup[F[_]: Monad : GenUUID : Logger](
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
                Circuit(acc.remainingGates ++ shiftedGates, acc.qubits + b.qubits) //TODO: Update this to merge gates based on slices 
            }}  

            private[qurator] def estimateFidelity[F[_]: Monad](
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

            private def fetchDeviceCalibration[F[_]: Monad](device: Device, clients: HttpClients[F]): F[DeviceCalibration] = device.platform match {
                case "IBM" => clients.ibm.fetchDeviceCalibration(device.platformId)
                case "Azure" => clients.azure.fetchDeviceCalibration(device.platformId)
                case "Braket" => clients.braket.fetchDeviceCalibration(device.platformId)
            } 

            // private[qurator] def buildGreedySynchronizedPlan[F[_]: Monad : Logger : MonadCancelThrow]( 
            //     orderedTasks: List[QuantumTask],
            //     candidatesByTask: Map[QuantumTask, List[CandidateDevice]],
            //     t1BudgetMillis: Long
            // ): F[SynchronizedPlan] = {
            //     final case class Acc(assignments: Map[Device, List[QuantumTask]], runtimeSum: Map[Device, Long])
            //     def taskStartFinish(device: Device, cand: CandidateDevice, acc: Acc): (Long, Long) = {
            //         val prev = acc.runtimeSum.getOrElse(device, 0L)
            //         val start = cand.queueMillis + prev
            //         val finish = start + cand.runMillis 
            //         (start, finish)
            //     }

            //     def objective(
            //         newStarts: List[Long],
            //         newFinishes: List[Long]
            //     ): Long = {
            //         val spreadStart = (newStarts.maxOption.getOrElse(0L)  - newStarts.minOption.getOrElse(0L))
            //         val spreadFinish = (newFinishes.maxOption.getOrElse(0L) - newFinishes.minOption.getOrElse(0L))
                    
            //         val t1Penalty = 
            //             if(t1BudgetMillis > 0L && spreadFinish > t1BudgetMillis) (spreadFinish - t1BudgetMillis) * 10L
            //             else 0L
                    
            //         spreadStart + (spreadFinish / 2L) + t1Penalty
            //     }

            //     def chooseBestDeviceForTask(t: QuantumTask, acc: Acc): F[(Device, CandidateDevice)] = { 
            //         val candidates = candidatesByTask.getOrElse(t, Nil)
            //         if(candidates.isEmpty){
            //             Logger[F].warn(s"No candidates computed for task=${t.uuid}; forcing enqueueReady") *>
            //             (new RuntimeException("No candidates for task")).raiseError[F, (Device, CandidateDevice)]
            //         }else{
            //             val currentDeviceQueuesWithinGroup: Map[Device, Long] =
            //                 candidatesByTask.values.flatten.map(c => c.device -> c.queueMillis).toMap
                        
            //             val currentStarts: List[Long] = 
            //                 acc.assignments.keys.toList.map{d =>
            //                     currentDeviceQueuesWithinGroup.getOrElse(d, 0L) + 0L
            //                 }

            //             val currentFinishes: List[Long] = 
            //                 acc.assignments.keys.toList.map{d => 
            //                     currentDeviceQueuesWithinGroup.getOrElse(d, 0L) + acc.runtimeSum.getOrElse(d, 0L)    
            //                 }

            //             candidates.traverse{cand => 
            //                 val device = cand.device
            //                 val (s0, f0) = taskStartFinish(device, cand, acc)

            //                 val starts2 = s0 :: currentStarts 
            //                 val finishes2 = f0 :: currentFinishes

            //                 val obj = objective(starts2, finishes2)
            //                 (obj, -cand.fidelity, cand.queueMillis, cand).pure[F]
            //             }.map{scored => 
            //                 val best = scored.minBy{case (obj, negFid, q, _) => (obj, negFid, q)}._4
            //                 (best.device, best)
            //             }
            //         }
            //     }

            //     orderedTasks.foldLeftM(Acc(Map.empty[Device, List[QuantumTask]], Map.empty[Device, Long])) { (acc, t) => 
            //         chooseBestDeviceForTask(t, acc).map{case (d, cand) => 
            //             val updatedAssignments = acc.assignments.updated(d, acc.assignments.getOrElse(d, Nil) :+ t)
            //             val updatedRuntimeSum = acc.runtimeSum.updated(d, acc.runtimeSum.getOrElse(d, 0L) + cand.runMillis) 
            //             Acc(updatedAssignments, updatedRuntimeSum)   
            //         }
            //     }.map(acc => SynchronizedPlan(acc.assignments))
            // }

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
