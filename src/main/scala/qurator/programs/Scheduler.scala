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

        private def submitSynronizedTaskRequest(str: SynronizedTaskRequest): F[Unit] = 
            for{
                tasks <- str.l.traverse{req => 
                    ID.make[F, TaskId].map{tid =>
                        Task(
                            tid,
                            req.taskType,
                            req.qasm, 
                            red.qubits,
                            req.shots,
                            req.depth,
                            req.parentTasks,
                            rep.childTasks,
                            req.createdAt
                        )    
                    }
                }

                possiblyMergedTasks <- attemptToMergeSyncTasks(tasks, targetEstimatedFidelity)
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


        private def attemptToMergeSyncTasks(
            task: List[Task],
            fidelityThreshold: Long
        ): F[List[Task]] = 
            for{
                devices <- getAvailableDevices()
                maxQubits = devices.map(_.qubits).maxOption.getOrElse(0)
                depthTolerance = 0.10 
                sorted = tasks.sortBy(_.qubits)
                groups = {
                    def similarDepth(a: Task, b: Task): Boolean = {
                        val depthDiff = Math.abs(a.depth.value - b.depth.value)
                        val avgDepth = (a.depth.value + b.depth.value).toDouble / 2.0
                        depthDiff.toDouble / avgDepth <= depthTolerance
                    }

                    sorted.foldLeft(List.empty[List[Task]]) { (acc, task) =>
                        acc match {
                            case Nil => List(List(task))
                            case head :: tail =>
                                if (head.nonEmpty && head.last.qubits.value + task.qubits.value <= maxQubits && similarDepth(head.last, task)) {
                                    (head :+ task) :: tail
                                } else {
                                    List(task) :: acc
                                }
                        }
                    }.map(_.reverse).reverse
                }

                mergedOrOG <- groups.traverse{ g => 
                    if(g.length >=2){
                        val mergedQubits = g.map(_.qubits).sum
                        val mergedDepth  = g.map(_.depth).max

                        val mergedCircuit = mergeCircuits(g.map(_.qasm.toCircuit))
                        val mergedCandidates = devices.filter(_.qubits >= mergedQubits)
                        
                        val mergedTask: F[Task] = 
                            ID.make[F, TaskId].map{tid => 
                                Task(
                                    uuid = tid,
                                    taskType = g.head.taskType,
                                    qasm = mergedCircuit.toQasm,
                                    qubits = mergedQubits,
                                    shots = g.map(_.shots).max,
                                    depth = mergedDepth,
                                    parentTasks = Nil, //obv not corret 
                                    childTasks = g.map(_.uuid),
                                    createdAt = g.map(_.createdAt).min
                                )
                            }

                        mergedTask.flatMap{mt =>
                            mergedCandidates.traverse{d =>
                                estimateFidelity(d, mt)    
                            }.map(_.maxOption.getOrElse(0L)).flatMap{
                                case bestF if bestF >= fidelityThreshold =>
                                    List(mt).pure[F]
                                case _ => 
                                    g.pure[F]
                            }

                        }
                    }else{
                        g.pure[F]
                    }
                }
            } yield mergedOrOG
            

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


        private def getAvailableDevices(): F[List[Any]] = ???

        private def allParentResultsAvailable() : Boolean = ??? 

        private def estimateTranspilationTime(circuit: Circuit, targetGateSet: List[Gate]) : F[Long] = ???

        private def requiresCutting(task: NewTaskRequest, devices: List[Any]) : F[Boolean] = ???

        private def enqueueReady(newTasks: List[Task]): F[Unit] =
            readyTasks.update(ts => prioritizationStrategy(newTasks ++ ts))

        private def enqueuePending(newTasks: List[Task]): F[Unit] =
            pendingTasks.update(ts => newTasks ++ ts)
    }
}