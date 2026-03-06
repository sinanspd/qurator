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

trait Scheduler[F[_]]{
    def submitTask(taskReq: TaskRequest): F[List[TaskId]]
    def estimateQueueTime(device: Device, task: QuantumTask) : F[Long]
    def getSubmittedTasks(): F[List[(String, String, TaskId)]]
}

object Scheduler{
  def make[F[_]: GenUUID: Concurrent: Logger : Temporal : Background](
        dataPersistanceService: DataPersistanceService[F],
        clients: HttpClients[F],
        prioritizationStrategy: List[Task] => List[Task],
        cuttingStrategy: Circuit => List[Circuit],
        targetEstimatedFidelity: Double, 
        additionalOptimizationRuns: Circuit => List[Circuit],
        compiler: FakeCompiler[F] //abstract this 
  ): F[Scheduler[F]] =
    for {
      readyTasks     <- Ref.of[F, List[Task]](List.empty)
      pendingTasks   <- Ref.of[F, List[Task]](List.empty)
      submittedTasks <- Ref.of[F, List[(String, String, TaskId)]](List.empty) 
      results        <- Ref.of[F, Map[TaskId, String]](Map.empty)        
    } yield new Scheduler[F] {

        private val idleDelay: FiniteDuration = 250.millis

       
        //TODO Merge Cut Task Results 
        //TODO for synronized tasks, can cutting be done more intelligently to isolate non-entangled parts?
        //TODO consider impact of cross talk when scheduling multiple tasks on the same device --> need topology aware mapping. Defined as avg distance between data qubits 
        //TODO Use estimateSynronizationCost to implement merging. Downside, this requires time estimation for classical tasks.
        //TODO: Estimate preperation time and add to queue time (and use entanglement estimation for runtime estimation)
        //TODO: Do we even need to estimate the queue? 
        //TODO: Loop back actual job data 
        //TODO: revisit Nick's comments 

        /////////////////////////////////////////////// NOT ADDRESSING NOW ////////////////////////////////////////////
        //TODO There is a possibility that merging tasks early limits the devices in the syncronization stage later on. 
        //TODO Fall back to other devices on failure (maybe after expontential backoff ?)
        //TODO think about reservations?? 
        //TODO add Result types (we can do this after the paper is done, dummy results for the sake of evaluation is fine for now) 
        //TODO: I think buildGreedySynchronizedPlan needs to be revised (chain scheduling issue)
        //TODO: We need to move some of the logic to supervisor so that the scheduler keeps running on error 
        //TODO: Stronger topology mapping 


        def submitTask(taskReq: TaskRequest): F[List[TaskId]] = taskReq match{
            case str : SynronizedQuantumTaskRequest => submitSynronizedTaskRequest(str)
            case ntr : NewQuantumTaskRequest => submitNewTaskRequest(ntr)
            case ctr : NewClassicalTaskRequest => submitClassicalTaskRequest(ctr)
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
                tids <- 
                    if(needsToBeCut){ //TODO: Think this through carefully. I am not convinced this is the right place to cut. 
                        val cut = cuttingStrategy(taskReq.circuit)
                        val optimized = cut.flatMap(additionalOptimizationRuns(_))
                        optimized.traverse { c =>                                          
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
                        }.flatMap { recreatedTasks =>
                            if (taskReq.parentTasks.nonEmpty) enqueuePending(recreatedTasks) *> List(recreatedTasks.map(_.uuid): _*).pure[F]
                            else enqueueReady(recreatedTasks) *> List(recreatedTasks.map(_.uuid): _*).pure[F]
                        }
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
                            if(taskReq.parentTasks.nonEmpty){enqueuePending(List(t)) *> List(t.uuid).pure[F]}
                            else {enqueueReady(List(t)) *> List(t.uuid).pure[F]}
                        })
                    }
            } yield tids 

        private def submitSynronizedTaskRequest(str: SynronizedQuantumTaskRequest): F[List[TaskId]] =  // TST
            for{
                cutTasks <- 
                    if (str.cut) {
                        str.l.traverse { req =>
                        requiresCutting(req, Nil).map { needsToBeCut =>
                            if (needsToBeCut) {
                                val cutCircuits       = cuttingStrategy(req.circuit)                  
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
                            } else {
                                List(req) 
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

                possiblyMergedTasks <- Scheduler.attemptToMergeSyncTasks(tasks, clients, compiler, targetEstimatedFidelity) 
                groupId <- ID.make[F, TaskId]
                sg = SyncronizedQuantumTaskList(
                    groupId,
                    possiblyMergedTasks,
                    str.t1Budget,
                    LocalDateTime.now()
                )
                allParents = str.l.foldLeft(List.empty[TaskId])((a, b) => a ++ b.parentTasks)
                _ <- if(allParents.isEmpty){enqueueReady(List(sg))}else{enqueuePending(List(sg))}
            }yield List(groupId)


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

            Concurrent[F].start(loop).void
        }

        private def scheduleOneQuantumTask(task: QuantumTask): F[Unit] = {
            for{
                devices <- Scheduler.getAvailableDevices(clients)
                suitableDevices = devices.filter(d => d.qubits >= task.qubits.value)
                _ <- suitableDevices match {
                    case Nil =>
                            Logger[F].warn(s"No eligible devices for task=${task.uuid}")
                    case ds => ds.traverse{d => 
                            (estimateFidelity(d, task.circuit, clients, compiler), estimateQueueTime(d, task), estimateTranspilationTime(task.circuit, d.gateSet)).mapN {
                                (f, q, t) =>
                                    val ac = getAssignmentCoefficient(f.logPTotal.toLong, q + t)
                                    (d, ac, f, q)
                            }
                        }
                }
            }yield ()
        }


        private def scheduleSynronizedTasks(s: SyncronizedQuantumTaskList): F[Unit] =
            for{
                devices <- Scheduler.getAvailableDevices(clients)
                orderedTasks = prioritizationStrategy(s.tasks).collect{ case t: QuantumTask => t}
                candidateDevicesByTask <- orderedTasks.traverse{t =>
                    val suitableDevices = devices.filter(d => d.qubits >= t.qubits.value)
                    suitableDevices.traverse{d => 
                        (estimateFidelity(d, t.circuit, clients, compiler), estimateQueueTime(d,t), estimateTranspilationTime(t.circuit, d.gateSet), estimateRunTime(d,t))
                            .mapN{(f, q, t, run) => 
                               val queueMillis = q + t
                               CandidateDevice(d, fidelity = f.logPTotal.toLong, queueMillis = queueMillis, runMillis = run)
                            }
                    }.map{cs => 
                        val possible = cs.filter(_.fidelity >= targetEstimatedFidelity)
                        if (possible.nonEmpty) possible else cs 
                    }.map(t -> _)   
                }.map(_.toMap)
                plan <- buildGreedySynchronizedPlan(
                    orderedTasks,
                    candidateDevicesByTask,
                    s.t1Budget
                )

                _ <- plan.assignments.toList.traverse_{case (device, tasksOnDevice) => 
                  tasksOnDevice.traverse_{t => 
                    submitJobWithFallback(device, t, candidateDevicesByTask.getOrElse(t, Nil))  
                  }    
                }
            }yield ()


        private def startFetchingResults(): F[Unit] =  
            Stream
                .repeatEval(fetchAllInProgressJobResults)
                .metered(scala.concurrent.duration.FiniteDuration(100, "ms")) 
                .compile
                .drain
        
        private def fetchAllInProgressJobResults(): F[Unit] = 
            submittedTasks.get.flatMap(sts => {
                sts.traverse_(st => fetchResultsFromCorrespondingProvider(st._1, st._2, st._3))
            }) *> ??? // promote pending to ready 
        
        //TODO: On Failure of job this needs to reschedule 
        private def fetchResultsFromCorrespondingProvider(provider: String, providerId: String, taskId: TaskId): F[Unit] = provider match{
            case "IBM" => clients.ibm.listJobDetails(providerId).map(r => r.status match {
                case "Completed" => 
                    results.get.flatMap(rm => results.set(rm + (taskId ->  "1"))) // dummy results for now 
                case _ => () 
            })
            case "Braket" => clients.braket.getQuantumTask(providerId).map(r => r.status match {
                case "COMPLETED" => 
                     results.get.flatMap(rm => results.set(rm + (taskId ->  "1"))) // dummy results for now 
                case _ => () 
            })
            case "Azure" => clients.azure.getQuantumTask(providerId).map(r => r.status match {
                case "Succeeded" => 
                    results.get.flatMap(rm => results.set(rm + (taskId ->  "1"))) // dummy results for now 
                case _ => ()
            })
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
        
        private def estimateSynronizationCost(tasks: List[QuantumTask]): F[Long] = ???  

        private def estimateRunTime(device: Device, task: QuantumTask) : F[Long] = ??? //might not be needed

        private def getAssignmentCoefficient(fidelity: Long, queueTime: Long): Double = 
            0.7 * fidelity + 0.9 * queueTime //adjust as needed
        

        private def requiresCutting(task: NewQuantumTaskRequest, devices: List[Device]) : F[Boolean] = 
            devices.traverse(d => Scheduler.estimateFidelity(d, task.circuit, clients, compiler)).map(lf => lf.filter(_.logPTotal > targetEstimatedFidelity).nonEmpty)

        ////////////////////////////////////////////////////////////
        private def allParentResultsAvailable(t: Task) : F[Boolean] = 
            results.get.map(rs => Scheduler.allParentResultsAvailable(rs, t))

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
            results.update(_ + (ct.uuid -> s"Result of classical task ${ct.uuid.value}"))
        }

        def getSubmittedTasks(): F[List[(String, String, TaskId)]] = 
            submittedTasks.get
    }


    private[qurator] def allParentResultsAvailable(
        results: Map[TaskId, String],
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

        private[qurator] def attemptToMergeSyncTasks[F[_] : Monad : GenUUID](
            tasks: List[QuantumTask],
            clients: HttpClients[F],
            compiler: FakeCompiler[F],
            targetEstimatedFidelity: Double
        ): F[List[QuantumTask]] = 
            for{
                devices <- getAvailableDevices[F](clients)
                maxQubits = devices.map(_.qubits).maxOption.getOrElse(0)
                depthTolerance = 0.10 
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

        private[qurator] def getAvailableDevices[F[_] : Monad](clients: HttpClients[F]): F[List[Device]] = 
            for {
                ibmDevices <- clients.ibm.fetchDeviceInformation
                availableIbmDevices = ibmDevices.devices.filter(d => d.status.name == "online").map(_.toDevice)
                braketDevices <- clients.braket.fetchDeviceList
                availableBraketDevices = braketDevices.devices.filter(d => d.deviceStatus == "ONLINE" && deviceActive(d)).map(_.toDevice)
                azureDevices <- clients.azure.fetchDeviceInformation
                availableAzureDevices = azureDevices.value.filter(d => d.currentAvailability == "Available").map(_.toDevice)
            } yield availableIbmDevices ++ availableBraketDevices ++ availableAzureDevices

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


        private[qurator] def flattenGroup[F[_]: Monad : GenUUID](
            group: List[QuantumTask], 
            devices: List[Device],
            clients: HttpClients[F],
            compiler: FakeCompiler[F],
            targetEstimatedFidelity: Double
        ): F[List[QuantumTask]] = 
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
                                    if (bestFidelity >= targetEstimatedFidelity) List(mergedTask).pure[F]
                                    else g.pure[F]
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
                    case U(start, end, power) => U(start + offset, end + offset, power)
                    case CU(ctrl, start, end, power) => CU(ctrl + offset, start + offset, end + offset, power)
                    case Swap(q1, q2) => Swap(q1 + offset, q2 + offset)
                    case CRotate(ctrl, thetaDenom, q) => CRotate(ctrl + offset, thetaDenom, q + offset)
                    case Rotate(thetaDenom, q) => Rotate(thetaDenom, q + offset)
                    case RZ(thetaDenom, q) => RZ(thetaDenom, q + offset)
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

            private[qurator] def buildGreedySynchronizedPlan[F[_]: Monad : Logger : MonadCancelThrow]( 
                orderedTasks: List[QuantumTask],
                candidatesByTask: Map[QuantumTask, List[CandidateDevice]],
                t1BudgetMillis: Long
            ): F[SynchronizedPlan] = {
                final case class Acc(assignments: Map[Device, List[QuantumTask]], runtimeSum: Map[Device, Long])
                def taskStartFinish(device: Device, cand: CandidateDevice, acc: Acc): (Long, Long) = {
                    val prev = acc.runtimeSum.getOrElse(device, 0L)
                    val start = cand.queueMillis + prev
                    val finish = start + cand.runMillis 
                    (start, finish)
                }

                def objective(
                    newStarts: List[Long],
                    newFinishes: List[Long]
                ): Long = {
                    val spreadStart = (newStarts.maxOption.getOrElse(0L)  - newStarts.minOption.getOrElse(0L))
                    val spreadFinish = (newFinishes.maxOption.getOrElse(0L) - newFinishes.minOption.getOrElse(0L))
                    
                    val t1Penalty = 
                        if(t1BudgetMillis > 0L && spreadFinish > t1BudgetMillis) (spreadFinish - t1BudgetMillis) * 10L
                        else 0L
                    
                    spreadStart + (spreadFinish / 2L) + t1Penalty
                }

                def chooseBestDeviceForTask(t: QuantumTask, acc: Acc): F[(Device, CandidateDevice)] = { 
                    val candidates = candidatesByTask.getOrElse(t, Nil)
                    if(candidates.isEmpty){
                        Logger[F].warn(s"No candidates computed for task=${t.uuid}; forcing enqueueReady") *>
                        (new RuntimeException("No candidates for task")).raiseError[F, (Device, CandidateDevice)]
                    }else{
                        val currentDeviceQueuesWithinGroup: Map[Device, Long] =
                            candidatesByTask.values.flatten.map(c => c.device -> c.queueMillis).toMap
                        
                        val currentStarts: List[Long] = 
                            acc.assignments.keys.toList.map{d =>
                                currentDeviceQueuesWithinGroup.getOrElse(d, 0L) + 0L
                            }

                        val currentFinishes: List[Long] = 
                            acc.assignments.keys.toList.map{d => 
                                currentDeviceQueuesWithinGroup.getOrElse(d, 0L) + acc.runtimeSum.getOrElse(d, 0L)    
                            }

                        candidates.traverse{cand => 
                            val device = cand.device
                            val (s0, f0) = taskStartFinish(device, cand, acc)

                            val starts2 = s0 :: currentStarts 
                            val finishes2 = f0 :: currentFinishes

                            val obj = objective(starts2, finishes2)
                            (obj, -cand.fidelity, cand.queueMillis, cand).pure[F]
                        }.map{scored => 
                            val best = scored.minBy{case (obj, negFid, q, _) => (obj, negFid, q)}._4
                            (best.device, best)
                        }
                    }
                }

                orderedTasks.foldLeftM(Acc(Map.empty[Device, List[QuantumTask]], Map.empty[Device, Long])) { (acc, t) => 
                    chooseBestDeviceForTask(t, acc).map{case (d, cand) => 
                        val updatedAssignments = acc.assignments.updated(d, acc.assignments.getOrElse(d, Nil) :+ t)
                        val updatedRuntimeSum = acc.runtimeSum.updated(d, acc.runtimeSum.getOrElse(d, 0L) + cand.runMillis) 
                        Acc(updatedAssignments, updatedRuntimeSum)   
                    }
                }.map(acc => SynchronizedPlan(acc.assignments))
            }
}