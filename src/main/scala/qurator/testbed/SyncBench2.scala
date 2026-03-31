package qurator.testbed

import cats.effect._
import cats.syntax.all._
import org.typelevel.log4cats.slf4j.Slf4jLogger

import qurator.domain.Task._
import qurator.domain.device._
import qurator.domain.circuit._
import qurator.effects.GenUUID
import qurator.domain.ID
import qurator.modules.HttpClients
import qurator.programs.Scheduler
import java.time.LocalDateTime
import org.typelevel.log4cats.Logger
import qurator.util.FidelityEstimator
import scala.util.Random

object SyncBench2 {

  implicit val logger = Slf4jLogger.getLogger[IO]

  sealed trait PathStage
  case object ClassicalStage extends PathStage
  final case class QuantumStage(spec: QuantumTaskSpec) extends PathStage

  final case class SyncLeafPlan(
    preSyncStages: List[PathStage],   
    syncSpec: QuantumTaskSpec
  )

  final case class RandomSyncTreeSpec(
    leaves: List[SyncLeafPlan],
    coherenceBudgetMillis: Long
   ) {
    require(leaves.nonEmpty, "RandomSyncTreeSpec.leaves must be non-empty")
  }

  final case class SyncGroupSpec(
    tasks: List[QuantumTaskSpec],
    coherenceBudgetMillis: Long
  ) {
    require(tasks.nonEmpty, "SyncGroupSpec.tasks must be non-empty")
  }


  final case class SyncSubmittedQuantum(
    taskId: TaskId,
    deviceId: String
  )

  final case class SyncTaskMetric(
    taskId: TaskId,
    deviceId: String,
    startMillis: Long,
    finishMillis: Long
  )

 final case class SyncGroupMetric(
  groupIndex: Int,
  groupSize: Int,
  coherenceBudgetMillis: Long,
  syncCostMillis: Long,
  startSkewMillis: Long,
  finishSkewMillis: Long,
  budgetViolationMillis: Long,
  survivalProxy: Double,
  taskMetrics: List[SyncTaskMetric]
) {
  lazy val budgetMet: Boolean =
    budgetViolationMillis == 0L
}

final case class SyncBenchmarkRun(
  policyName: String,
  groupsScheduled: Int,
  totalQuantumTasks: Int,
  schedulingWallMillis: Long,
  groupMetrics: List[SyncGroupMetric]
) {
  lazy val groupsPerSec: Double =
    if (schedulingWallMillis <= 0L) groupsScheduled.toDouble
    else groupsScheduled.toDouble / (schedulingWallMillis.toDouble / 1000.0)

  lazy val quantumTasksPerSec: Double =
    if (schedulingWallMillis <= 0L) totalQuantumTasks.toDouble
    else totalQuantumTasks.toDouble / (schedulingWallMillis.toDouble / 1000.0)

  lazy val meanSyncCostMillis: Double =
    if (groupMetrics.isEmpty) 0.0
    else groupMetrics.map(_.syncCostMillis.toDouble).sum / groupMetrics.size.toDouble

  lazy val meanStartSkewMillis: Double =
    if (groupMetrics.isEmpty) 0.0
    else groupMetrics.map(_.startSkewMillis.toDouble).sum / groupMetrics.size.toDouble

  lazy val meanFinishSkewMillis: Double =
    if (groupMetrics.isEmpty) 0.0
    else groupMetrics.map(_.finishSkewMillis.toDouble).sum / groupMetrics.size.toDouble

  lazy val meanBudgetViolationMillis: Double =
    if (groupMetrics.isEmpty) 0.0
    else groupMetrics.map(_.budgetViolationMillis.toDouble).sum / groupMetrics.size.toDouble

  lazy val budgetMetRate: Double =
    if (groupMetrics.isEmpty) 0.0
    else groupMetrics.count(_.budgetMet).toDouble / groupMetrics.size.toDouble

  lazy val meanSurvivalProxy: Double =
    if (groupMetrics.isEmpty) 0.0
    else groupMetrics.map(_.survivalProxy).sum / groupMetrics.size.toDouble
}

  sealed trait SyncBaselinePolicy {
    def name: String
  }

private def randomQuantumSpecFromPool(
  rng: scala.util.Random,
  pool: Vector[QuantumTaskSpec]
): QuantumTaskSpec =
  pool(rng.nextInt(pool.size))

private def randomPreSyncStages(
  minDepth: Int,
  maxDepth: Int,
  rng: scala.util.Random,
  branchQuantumPool: Vector[QuantumTaskSpec]
): List[PathStage] = {
  require(minDepth >= 1, "minDepth must be >= 1")
  require(maxDepth >= minDepth, "maxDepth must be >= minDepth")
  require(branchQuantumPool.nonEmpty, "branchQuantumPool must be non-empty")

  val depth = minDepth + rng.nextInt(maxDepth - minDepth + 1)

  // start with all-classical stages
  val initial = Vector.fill(depth)(ClassicalStage: PathStage)

  // only allow quantum insertion away from the last stage,
  // so the immediate parent of the sync task remains classical
  val candidateIdxs = rng.shuffle((0 until math.max(1, depth - 1)).toList)

  val finalStages =
    candidateIdxs.foldLeft((initial, Set.empty[Int])) {
      case ((stages, blocked), idx) =>
        val canPlaceQuantum =
          !blocked.contains(idx) &&
          !blocked.contains(idx - 1) &&
          !blocked.contains(idx + 1) &&
          rng.nextBoolean()

        if (canPlaceQuantum) {
          val q = QuantumStage(randomQuantumSpecFromPool(rng, branchQuantumPool))
          (
            stages.updated(idx, q),
            blocked ++ Set(idx - 1, idx, idx + 1)
          )
        } else {
          (stages, blocked)
        }
    }._1

  // force final stage classical
  finalStages.updated(finalStages.size - 1, ClassicalStage).toList
}

def attachRandomParentsToSyncGroups(
  syncGroups: List[SyncGroupSpec],
  minDepth: Int,
  maxDepth: Int,
  branchQuantumPool: Vector[QuantumTaskSpec],
  rng: scala.util.Random = new scala.util.Random()
): List[RandomSyncTreeSpec] = {
  require(branchQuantumPool.nonEmpty, "branchQuantumPool must be non-empty")

  syncGroups.map { group =>
    RandomSyncTreeSpec(
      leaves = group.tasks.map { syncTask =>
        SyncLeafPlan(
          preSyncStages = randomPreSyncStages(
            minDepth = minDepth,
            maxDepth = maxDepth,
            rng = rng,
            branchQuantumPool = branchQuantumPool
          ),
          syncSpec = syncTask
        )
      },
      coherenceBudgetMillis = group.coherenceBudgetMillis
    )
  }
}

def generateRandomSyncTreeSpec(
  fanout: Int,
  minDepth: Int,
  maxDepth: Int,
  coherenceBudgetMillis: Long,
  syncPool: Vector[QuantumTaskSpec],
  branchQuantumPool: Vector[QuantumTaskSpec],
  rng: Random = new Random()
): RandomSyncTreeSpec = {
  require(fanout >= 1, "fanout must be >= 1")
  require(syncPool.nonEmpty, "syncPool must be non-empty")

  val leaves =
    List.fill(fanout) {
      SyncLeafPlan(
        preSyncStages = randomPreSyncStages(minDepth, maxDepth, rng, branchQuantumPool),
        syncSpec = randomQuantumSpecFromPool(rng, syncPool)
      )
    }

  RandomSyncTreeSpec(
    leaves = leaves,
    coherenceBudgetMillis = coherenceBudgetMillis
  )
}

private def expectSingleId[A](label: String, ids: List[TaskId]): IO[TaskId] =
  ids match {
    case id :: Nil => id.pure[IO]
    case other =>
      new RuntimeException(s"$label expected exactly one TaskId, got ${other.size}")
        .raiseError[IO, TaskId]
  }

private def submitClassicalNode(
  scheduler: Scheduler[IO],
  parents: List[TaskId]
): IO[TaskId] =
  scheduler
    .submitTask(
      NewClassicalTaskRequest(
        program = (),
        parentTasks = parents,
        childTasks = Nil,
        createdAt = LocalDateTime.now()
      )
    )
    .flatMap(ids => expectSingleId("classical node", ids))

private def submitQuantumNode(
  scheduler: Scheduler[IO],
  spec: QuantumTaskSpec,
  parents: List[TaskId]
): IO[TaskId] =
  scheduler
    .submitTask(
      NewQuantumTaskRequest(
        circuit = spec.circuit,
        qubits = spec.qubits,
        shots = spec.shots,
        depth = spec.depth,
        parentTasks = parents,
        childTasks = Nil,
        createdAt = LocalDateTime.now()
      )
    )
    .flatMap(ids => expectSingleId("quantum node", ids))

private def submitBranchToSyncParent(
  scheduler: Scheduler[IO],
  sharedRootId: TaskId,
  stages: List[PathStage]
): IO[TaskId] =
  stages.foldLeftM(List(sharedRootId)) {
    case (currentParents, ClassicalStage) =>
      submitClassicalNode(scheduler, currentParents).map(id => List(id))

    case (currentParents, QuantumStage(spec)) =>
      for {
        qId <- submitQuantumNode(scheduler, spec, currentParents)
        cId <- submitClassicalNode(scheduler, List(qId))
      } yield List(cId)
  }.flatMap(ids => expectSingleId("branch terminal classical parent", ids))

final case class SubmittedSyncGroup(
  groupIndex: Int,
  coherenceBudgetMillis: Long,
  taskIdsInOrder: List[TaskId],
  parentIdsInOrder: List[List[TaskId]]
) {
  lazy val expectedIds: Set[TaskId] = taskIdsInOrder.toSet
}

private def submitRandomTreeGroup(
  scheduler: Scheduler[IO],
  groupIndex: Int,
  spec: RandomSyncTreeSpec
): IO[SubmittedSyncGroup] =
  for {
    rootId <- submitClassicalNode(scheduler, Nil)

    terminalParents <- spec.leaves.traverse { leaf =>
      submitBranchToSyncParent(scheduler, rootId, leaf.preSyncStages)
    }

    syncReq =
      SynronizedQuantumTaskRequest(
        l = spec.leaves.zip(terminalParents).map { case (leaf, parentId) =>
          NewQuantumTaskRequest(
            circuit = leaf.syncSpec.circuit,
            qubits = leaf.syncSpec.qubits,
            shots = leaf.syncSpec.shots,
            depth = leaf.syncSpec.depth,
            parentTasks = List(parentId),
            childTasks = Nil,
            createdAt = LocalDateTime.now()
          )
        },
        t1Budget = spec.coherenceBudgetMillis,
        cut = false
      )

    syncIds <- scheduler.submitTask(syncReq)

    // enforce "succeeded by 1 classical task" for each synchronized quantum member
    _ <- syncIds.traverse_ { qid =>
      scheduler.submitTask(
        NewClassicalTaskRequest(
          program = (),
          parentTasks = List(qid),
          childTasks = Nil,
          createdAt = LocalDateTime.now()
        )
      ).void
    }
  } yield SubmittedSyncGroup(
    groupIndex = groupIndex,
    coherenceBudgetMillis = spec.coherenceBudgetMillis,
    taskIdsInOrder = syncIds,
    parentIdsInOrder = terminalParents.map(pid => List(pid))
  )

private def buildSubmittedTaskMetricsForGroup(
  assignmentsInOrder: List[(TaskId, String, QuantumTaskSpec)],
  registry: BenchmarkDeviceRegistry
): IO[List[SyncTaskMetric]] = {

  def estimateRunMillis(device: Device, spec: QuantumTaskSpec): IO[Long] = {
    val rawCal = registry.calibration(device.platformId)
    val cal = FidelityEstimator.normalizeCalibration(rawCal)

    val totalGateDurationNs =
      spec.circuit.remainingGates.foldLeft(0L) { (acc, gate) =>
        acc + cal.durationNsFor(gate)
      }

    val gateDurationMillis =
      math.ceil(totalGateDurationNs.toDouble / 1_000_000.0).toLong

    val preparationMillis = 3000L

    (preparationMillis + gateDurationMillis).pure[IO]
  }

  val groupedByDevice: Map[String, List[(TaskId, String, QuantumTaskSpec)]] =
    assignmentsInOrder.groupBy(_._2)

  groupedByDevice.toList.flatTraverse { case (deviceId, devAssignments) =>
    val device = registry.device(deviceId)

    devAssignments.foldLeftM((0L, List.empty[SyncTaskMetric])) {
      case ((accumulatedRunMillis, acc), (taskId, _, spec)) =>
        for {
          rMillis <- estimateRunMillis(device, spec)

          qMillis = queueWaitMillis(deviceId, spec, registry)

          startMillis = qMillis + accumulatedRunMillis
          finishMillis = startMillis + rMillis

          m = SyncTaskMetric(
            taskId = taskId,
            deviceId = deviceId,
            startMillis = startMillis,
            finishMillis = finishMillis
          )
        } yield (accumulatedRunMillis + rMillis, acc :+ m)
    }.map(_._2)
  }
}

private def buildGroupMetric(
  groupIndex: Int,
  coherenceBudgetMillis: Long,
  syncCostMillis: Long,
  taskMetrics: List[SyncTaskMetric]
): SyncGroupMetric = {
  val starts = taskMetrics.map(_.startMillis)
  val finishes = taskMetrics.map(_.finishMillis)

  val startSkewMillis =
    if (starts.isEmpty) 0L else starts.max - starts.min

  val finishSkewMillis =
    if (finishes.isEmpty) 0L else finishes.max - finishes.min

  val budgetViolationMillis =
    math.max(0L, finishSkewMillis - coherenceBudgetMillis)

  val survivalProxy =
    if (coherenceBudgetMillis <= 0L) 0.0
    else math.exp(-finishSkewMillis.toDouble / coherenceBudgetMillis.toDouble)

  SyncGroupMetric(
    groupIndex = groupIndex,
    groupSize = taskMetrics.size,
    coherenceBudgetMillis = coherenceBudgetMillis,
    syncCostMillis = syncCostMillis,
    startSkewMillis = startSkewMillis,
    finishSkewMillis = finishSkewMillis,
    budgetViolationMillis = budgetViolationMillis,
    survivalProxy = survivalProxy,
    taskMetrics = taskMetrics
  )
}

private def computeSyncCostMillis(
  parentIdsInOrder: List[List[TaskId]],
  results: Map[TaskId, (String, Long)]
): IO[Long] =
  parentIdsInOrder.traverse { pids =>
    if (pids.isEmpty) 0L.pure[IO]
    else {
      val times = pids.flatMap(pid => results.get(pid).map(_._2))
      if (times.size != pids.size) {
        new RuntimeException(
          s"Missing completion timestamp for one or more sync parents: ${pids.mkString(", ")}"
        ).raiseError[IO, Long]
      } else {
        times.max.pure[IO]
      }
    }
  }.map { releaseTimes =>
    if (releaseTimes.isEmpty) 0L
    else releaseTimes.max - releaseTimes.min
  }

object SyncBaselinePolicy {
    case object IndependentLeastBusy extends SyncBaselinePolicy {
      val name = "independent_least_busy"
    }
    case object IndependentHighestFidelity extends SyncBaselinePolicy {
      val name = "independent_highest_fidelity"
    }
  }

  private def monotonicMillis: IO[Long] = //thing like this is shared with the regular benchmarks, need to refactor these 
    Temporal[IO].monotonic.map(_.toMillis)

  private def gateCount(c: Circuit): Long =
    math.max(1L, c.remainingGates.size.toLong)

  private def queueWaitMillis(
    deviceId: String,
    spec: QuantumTaskSpec,
    registry: BenchmarkDeviceRegistry
  ): Long =
    registry.queueLen(deviceId).toLong * gateCount(spec.circuit) * registry.msPerGate

  private def runMillis(
    spec: QuantumTaskSpec,
    registry: BenchmarkDeviceRegistry
  ): Long =
    gateCount(spec.circuit) * registry.msPerGate

  private def resolveDeviceId(
    registry: BenchmarkDeviceRegistry,
    s1: String,
    s2: String
  ): String =
    if (registry.devicesById.contains(s1)) s1
    else if (registry.devicesById.contains(s2)) s2
    else s1

  private def buildGroupTaskMetrics(
    assignments: List[(TaskId, String, QuantumTaskSpec)],
    registry: BenchmarkDeviceRegistry,
    clients: HttpClients[IO],
    compiler: FakeCompiler[IO]
  ): IO[List[SyncTaskMetric]] = {

    val groupedByDevice: Map[String, List[(TaskId, String, QuantumTaskSpec)]] =
      assignments.groupBy(_._2)

    groupedByDevice.toList.flatTraverse { case (deviceId, devAssignments) =>
      val baseQueueForDevice: Long =
        registry.queueLen(deviceId).toLong

      devAssignments.foldLeftM((0L, List.empty[SyncTaskMetric])) {
        case ((accumulatedRunMillis, acc), (taskId, _, spec)) =>
          val qMillis = queueWaitMillis(deviceId, spec, registry)
          val rMillis = runMillis(spec, registry)

          val startMillis  = qMillis + accumulatedRunMillis
          val finishMillis = startMillis + rMillis

          val device = registry.device(deviceId)

          Scheduler
            .estimateFidelity[IO](device, spec.circuit, clients, compiler)
            .map { est =>
              val m = SyncTaskMetric(
                taskId = taskId,
                deviceId = deviceId,
                startMillis = startMillis,
                finishMillis = finishMillis,
              )
              (accumulatedRunMillis + rMillis, acc :+ m)
            }
      }.map(_._2)
    }
  }

private def waitUntilAllSubmitted(
    scheduler: Scheduler[IO],
    expectedIds: Set[TaskId],
    pollEvery: scala.concurrent.duration.FiniteDuration
): IO[List[SyncSubmittedQuantum]] = {

    def loop(seen: Map[TaskId, SyncSubmittedQuantum]): IO[List[SyncSubmittedQuantum]] =
        scheduler.getSubmittedTasks().flatMap { raw =>
            val newlySeen: List[SyncSubmittedQuantum] =
                raw.flatMap {
                    case (_, platformId, _, tid) =>
                        if (!expectedIds.contains(tid)) None
                        else Some(SyncSubmittedQuantum(tid, platformId))
                }

            val mergedSeen: Map[TaskId, SyncSubmittedQuantum] =
                newlySeen.foldLeft(seen) { (acc, sq) =>
                    acc.updated(sq.taskId, sq)
                }

            val remaining = expectedIds.diff(mergedSeen.keySet)

            // Logger[IO].info(
            //     s"Waiting For All Submitted To Finish. Seen=${mergedSeen.keySet.size}/${expectedIds.size}. Remaining=${remaining.mkString(", ")}"
            // ) *>
            (
                if (remaining.isEmpty) mergedSeen.values.toList.pure[IO]
                else Temporal[IO].sleep(pollEvery) *> loop(mergedSeen)
            )
        }

    if (expectedIds.isEmpty) List.empty[SyncSubmittedQuantum].pure[IO]
    else loop(Map.empty)
}

def runSchedulerSyncTreeBenchmark(
  scheduler: Scheduler[IO],
  trees: List[RandomSyncTreeSpec],
  registry: BenchmarkDeviceRegistry,
  clients: HttpClients[IO],
  compiler: FakeCompiler[IO],
  pollEvery: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.DurationInt(100).millis
): IO[SyncBenchmarkRun] =
  for {
    t0 <- monotonicMillis

    submittedGroups <- trees.zipWithIndex.traverse { case (tree, idx) =>
      submitRandomTreeGroup(
        scheduler = scheduler,
        groupIndex = idx,
        spec = tree
      )
    }

    expectedIds = submittedGroups.flatMap(_.expectedIds).toSet

    //_ <- Logger[IO].info("Waiting For All Submitted To Finish")
    submitted <- waitUntilAllSubmitted(
      scheduler = scheduler,
      expectedIds = expectedIds,
      pollEvery = pollEvery
    )
    _ <- Logger[IO].info("All Submitted Finished")

    submittedById: Map[TaskId, String] =
      submitted.map(s => s.taskId -> s.deviceId).toMap

    results <- scheduler.getResults()

    groupMetrics <- submittedGroups.zip(trees).traverse { case (sg, tree) =>
      val assignmentsInOrder: List[(TaskId, String, QuantumTaskSpec)] =
        sg.taskIdsInOrder.zip(tree.leaves.map(_.syncSpec)).collect {
          case (taskId, spec) if submittedById.contains(taskId) =>
            (taskId, submittedById(taskId), spec)
        }

      for {
        syncCost <- computeSyncCostMillis(
          parentIdsInOrder = sg.parentIdsInOrder,
          results = results
        )

        taskMetrics <- buildSubmittedTaskMetricsForGroup(
          assignmentsInOrder = assignmentsInOrder,
          registry = registry
        )
      } yield buildGroupMetric(
        groupIndex = sg.groupIndex,
        coherenceBudgetMillis = sg.coherenceBudgetMillis,
        syncCostMillis = syncCost,
        taskMetrics = taskMetrics
      )
    }

    t1 <- monotonicMillis
  } yield SyncBenchmarkRun(
    policyName = "scheduler_sync_tree",
    groupsScheduled = trees.size,
    totalQuantumTasks = trees.map(_.leaves.size).sum,
    schedulingWallMillis = t1 - t0,
    groupMetrics = groupMetrics
  )

  private def chooseIndependentLeastBusyDevice(
    spec: QuantumTaskSpec,
    registry: BenchmarkDeviceRegistry
  ): IO[Device] = {
    val feasible = registry.devices.filter(_.qubits >= spec.qubits.value)
    if (feasible.isEmpty) {
      new RuntimeException("No feasible device for sync task").raiseError[IO, Device]
    } else {
      feasible
        .map { d =>
          val qMillis = queueWaitMillis(d.platformId, spec, registry)
          (qMillis, d)
        }
        .minBy(_._1)
        ._2
        .pure[IO]
    }
  }

  private def chooseIndependentHighestFidelityDevice(
    spec: QuantumTaskSpec,
    registry: BenchmarkDeviceRegistry,
    clients: HttpClients[IO],
    compiler: FakeCompiler[IO]
  ): IO[Device] = {
    val feasible = registry.devices.filter(_.qubits >= spec.qubits.value)
    if (feasible.isEmpty) {
      new RuntimeException("No feasible device for sync task").raiseError[IO, Device]
    } else {
      feasible
        .traverse(d => Scheduler.estimateFidelity[IO](d, spec.circuit, clients, compiler).map(est => (est.logPTotal, d)))
        .map(_.maxBy(_._1)._2)
    }
  }


}