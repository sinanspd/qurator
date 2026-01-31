package qurator.programs

import cats.MonadThrow
import cats.effect._
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import qurator.service.DataPersistanceService
import scala.util.Random
import qurator.domain.Task._
import qurator.effects.GenUUID
import com.sinanspd.qure.circuit.Circuit
import com.sinanspd.qure.circuit.gates.Gate
import cats.effect.kernel.Ref
import qurator.domain.ID
import scala.concurrent.duration._
import qurator.domain.device.Device


trait Scheduler[F[_]]{
    def submitTask(taskReq: TaskRequest): F[Unit]
}

object Scheduler{
    def make[F[_]: GenUUID: Concurrent: Logger : Temporal](
        dataPersistanceService: DataPersistanceService[F],
        prioritizationStrategy: List[Task] => List[Task],
        cuttingStrategy: Circuit => List[Circuit],
        targetEstimatedFidelity: Long, 
        additionalOptimizationRuns: Circuit => List[Circuit]
  ): F[Scheduler[F]] =
    for {
      readyTasks     <- Ref.of[F, List[Task]](List.empty)
      pendingTasks   <- Ref.of[F, List[Task]](List.empty)
      submittedTasks <- Ref.of[F, List[(String, String)]](List.empty) 
      results        <- Ref.of[F, Map[TaskId, String]](Map.empty)        
    } yield new Scheduler[F] {

        private val idleDelay: FiniteDuration = 250.millis

        //TODO Fall back to other devices on failure 
        //TODO Merge Cut Task Results 
        //TODO Do we really need to account for classial communication cost? Will it not be more or less constant in this setting? 
        //TODO think about reservations?? 
        //TODO Synched tasks should be cut as well?? 
        //TODO consider impact of cross talk when scheduling multiple tasks on the same device.

        def submitTask(taskReq: TaskRequest): F[Unit] = taskReq match{
            case str : SynronizedQuantumTaskRequest => submitSynronizedTaskRequest(str)
            case ntr : NewQuantumTaskRequest => submitNewTaskRequest(ntr)
            case ctr : NewClassicalTaskRequest => submitClassicalTaskRequest(ctr)
        }

        private def submitClassicalTaskRequest(taskReq: NewClassicalTaskRequest): F[Unit] = 
            ID.make[F, TaskId].flatMap(taskId => {
                val t = ClassicalTask( 
                        uuid = taskId,
                        program = taskReq.program,
                        parentTasks = taskReq.parentTasks,
                        childTasks = taskReq.childTasks,
                        createdAt = taskReq.createdAt
                )
                if(taskReq.parentTasks.isEmpty || allParentResultsAvailable()){
                    enqueueReady(List(t))
                }else{
                    enqueuePending(List(t))
                }
            })


        private def submitNewTaskRequest(taskReq: NewQuantumTaskRequest): F[Unit] = 
             for{
                devices <- getAvailableDevices()
                needsToBeCut <- requiresCutting(taskReq, devices)
                _ <- 
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
                            if (taskReq.parentTasks.nonEmpty) enqueuePending(recreatedTasks)
                            else enqueueReady(recreatedTasks)
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
                            if(taskReq.parentTasks.nonEmpty){enqueuePending(List(t))}
                            else {enqueueReady(List(t))}
                        })
                    }
            } yield () 

        private def submitSynronizedTaskRequest(str: SynronizedQuantumTaskRequest): F[Unit] = 
            for{
                tasks <- str.l.traverse{req => 
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

                possiblyMergedTasks <- attemptToMergeSyncTasks(tasks)
                groupId <- ID.make[F, SyncGroupId]
                sg = SychronizedTaskList(
                    groupId,
                    mergedTasks,
                    str.t1BudgetMillis,
                    str.createdAt
                )
                allParents = str.l.foldLeft(List.empty)((a, b) => a.parentTasks ++ b)
                _ <- if(allParents.isEmpty){enqueueReady(sg)}else{enqueuePending(sg)}
            }yield ()

        def startScheduling(): F[Unit] = {
            def pickNextReady: F[Option[Task]] =
                readyTasks.modify {
                    case h :: t => (t, Some(h))
                    case Nil    => (Nil, None)
                }

            def scheduleOnce(task: Task): F[Unit] = 
                for{
                    devices <- getAvailableDevices()
                    suitableDevices = devices.filter(d => d.qubits >= task.qubits)
                    _ <- suitableDevices match {
                        case Nil =>
                            Logger[F].warn(s"No eligible devices for task=${task.uuid}")
                        case ds => ds.traverse{d => 
                                (estimateFidelity(d, task), estimateQueue(d, task) + estimateTranspilationTime(task.circuit, d.gateSet)).mapN {
                                    (f, q) =>
                                    val ac = deviceOps.getAssignmentCoefficient(f, q)
                                    (d, ac, f, q)
                                }
                            }.flatMap { scored => 
                                val maxCoeff = scored.maxBy(_._2)
                                ???
                            }
                    }
                } yield ()

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

        private def scheduleSynronizedTasks(s: SynchronizedTaskList): F[Unit] =
            for{
                devices <- getAvailableDevices()
                orderedTasks = prioritizationStrategy(s.tasks)
                candidateDevicesByTask <- orderedTasks.traverse{t =>
                    val suitableDevices = devices.filter(d => d.qubits >= t.qubits)
                    suitableDevices.traverse{d => 
                        (d, estimateFidelity(d,t), estimateQueue(d,t) + estimateTranspilationTime(t.circuit, d.gateSet), estimateRunTime(d,t))
                            .mapN{(d, f, q, run, transp) => 
                                ???
                            }
                    }.map{cs => 
                        val possible = cs.filter(_.fidelity >= targetEstimatedFidelity)
                        if (possible.nonEmpty) possible else cs 
                    }   
                }

                plan <- buildGreedySynchronizedPlan(
                    orderedTasks,
                    candidatedByTask,
                    t1BudgetMillis
                )

                _ <- plan.assignments.toList.traverse_{case (device, taskOnDevice) => 
                  tasksOnDevice.traverse_{t => 
                    submitJobWithFallback(device, t, candidatesByTask.getOrElse(t, Nil))  
                  }    
                }
            }yield ()

        private def startFetchingResults(): F[Unit] =  ??? 

        private def buildGreedySynchronizedPlan(
            orderedTasks: List[Task],
            candidatesByTask: Map[Task, List[???]],
            t1BudgetMillis: Long
        ): F[SynchronizedPlan] = {
            final case class Acc(assignments: Map[Device, List[Task]], runtimeSum: Map[Device, Long])

            def taskStartFinish(device: Device, cand: Candidate, acc: Acc): (Long, Long) = {
                val prev = acc.runtimeSum.getOrElse(device, 0L)
                val start = can.queueMillis + prev
                val finish = start + cand.runMillis 
                (start, finish)
            }

            def objective(
                newStarts: List[Long],
                newFinishes: List[Long]
            ): Long = {
                val spreadStart = (newStarts.maxOption.getOrElse(0L)  - newStarts.minOption.getOrElse(0L))
                val spreadFinish = (newFinishes.maxOption.getOrElse(0L) - newFinishes.minOption.getOrElse(0L))
                
                // penalty for exceeding T1 
                val t1Penalty = 
                    if(t1BudgetMillis > 0L && spreadFinish > t1BudgetMillis) (spreadFinish - t1BudgetMillis) * 10L
                    else 0L
                
                spreadStart + (spreadFinish / 2L) +t1Penalty
            }

            def chooseBestDeviceForTask(t: Task, acc: Acc): F[(Device, Candidate)] = {
                val candidates = candidatesByTask.getOrElse(t, Nil)
                if(candidates.isEmpty){
                    Logger[F].warn(s"No candidates computed for task=${t.uuid}; forcing enqueueReady") *>
                    (new RuntimeException("No candidates for task")).raiseError[F, (Device, Candidate)]
                }else{
                    val currentDeviceQueuesWithinGroup: Map[Device, Long] =
                        candidatesByTask.values.flatten.map(c => c.device -> c.queueMillis).toMap
                    
                    val currentStarts: List[Long] = 
                        acc.assignments.keys.toList.map{d =>
                            currentDeviceQueuesWithinGroup.getOrElse(d, 0L) + 0L
                        }

                    val currentFinishes: List[Long] = 
                        acc.assignments.keys.toList.map{d => 
                            currentDeviceQueues.getOrElse(d, 0L) + acc.runtimeSum.getOrElse(d, 0L)    
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

            orderedTasks.foldLeftM(Acc(Map.empty, Map.empty)) { (acc, t) => 
                chooseBestDeviceForTask(t, acc).map{case (d, cand) => 
                  val updatedAssignments = acc.assignments.updated(d, acc.assignments.getOrElse(d, Nil) :+ t)
                  val updatedRuntimeSum = acc.runtimeSum.updated(d, acc.runtimeSum.getOrElse(d, 0L) + cand.runMillis) 
                  Acc(updatedAssignments, updatedRuntimeSum)   
                }
            }.map(acc => SynchronizedPlan(acc.assignments))
        }



        /**
         * The general idea behind merging is this:
         * The goal is to attempt to merge tasks that are similar in depth 
         * and can fit on the same device while maintaining high fidelity 
         * Not all tasks will be merged but more than one task can be merged into one
         * This obv. turns into an exponential problem if we try all possible merge combinations 
         *
         * */

        private def attemptToMergeSyncTasks(
            tasks: List[QuantumTask]
        ): F[List[QuantumTask]] = 
            for{
                devices <- getAvailableDevices()
                maxQubits = devices.map(_.qubits).maxOption.getOrElse(0)
                depthTolerance = 0.10 
                depthBuckets = bucketByDepth(tasks, depthTolerance)
                groups = depthBuckets.flatMap { bucket =>
                    assignToFinalBuckets(
                        bucket = bucket,
                        capacity = maxQubits, 
                        maxTasksPerBin = 3 //fix this, obv shouldn't be constant
                    )
                }
                merged <- groups.traverse(g => flattenGroup(g, devices))
            } yield merged.flatten

      

        private def bucketByDepth(tasks: List[QuantumTask], depthRelTol: Double): List[List[QuantumTask]] = {
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

        private def assignToFinalBuckets(
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

        private def flattenGroup(group: List[QuantumTask], devices: List[Device]): F[List[QuantumTask]] =
            group match {
                case Nil          => List.empty[QuantumTask].pure[F]
                case single :: Nil => List(single).pure[F]
                case g =>
                    val mergedQubits = g.map(_.qubits.value).sum
                    val feasibleDevices = devices.filter(_.qubits >= mergedQubits)

                    if (feasibleDevices.isEmpty) {
                        g.pure[F]
                    } else {
                        val mergedCircuit: com.sinanspd.qure.circuit.Circuit =
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
                                .traverse(d => estimateFidelity(d, mergedTask)) 
                                .map(_.maxOption.getOrElse(0L))
                                .flatMap { bestFidelity =>
                                    if (bestFidelity >= targetEstimatedFidelity) List(mergedTask).pure[F]
                                    else g.pure[F]
                                }
                            }
                    }
            }            

    //   def startScheduling(): F[Unit] =
    //     Stream
    //       .repeatEval(scheduleNextTask())
    //       .metered(scala.concurrent.duration.FiniteDuration(100, "ms"))
    //       .compile
    //       .drain

    //   private def scheduleOnce(): F[Unit] =
    //     for {
    //       tasks <- readyTasksRef.get
    //       _ <- tasks.headOption match {
    //         case Some(task) =>
    //           for {
    //             _ <- scheduleTask(task)
    //             _ <- readyTasksRef.update(_.tail)
    //           } yield ()
    //         case None =>
    //           Concurrent[F].unit
    //       }
    //     } yield ()


        private def getAvailableDevices(): F[List[Device]] = ???

        private def allParentResultsAvailable() : Boolean = ??? 

        private def estimateTranspilationTime(circuit: Circuit, targetGateSet: List[Gate]) : F[Long] = ???

        private def estimateFidelity(device: Device, task: QuantumTask) : F[Long] = ???

        private def estimateQueueTime(device: Device, task: QuantumTask) : F[Long] = ???

        private def requiresCutting(task: NewQuantumTaskRequest, devices: List[Any]) : F[Boolean] = ???

        private def enqueueReady(newTasks: List[Task]): F[Unit] =
            readyTasks.update(ts => prioritizationStrategy(newTasks ++ ts))

        private def enqueuePending(newTasks: List[Task]): F[Unit] =
            pendingTasks.update(ts => newTasks ++ ts)

        private def mergeCircuits(circuits: List[Circuit]): Circuit = ???
    }
}