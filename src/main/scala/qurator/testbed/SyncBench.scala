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

object SyncBench {

  implicit val logger = Slf4jLogger.getLogger[IO]

  final case class SyncGroupSpec(
    tasks: List[QuantumTaskSpec],
    coherenceBudgetMillis: Long
  ) {
    require(tasks.nonEmpty, "SyncGroupSpec.tasks must be non-empty")
  }

  final case class SubmittedSyncGroup(
    groupIndex: Int,
    coherenceBudgetMillis: Long,
    expectedIds: Set[TaskId]
  )

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

private def buildSubmittedTaskMetricsForGroup(
  groupSubmitted: List[SyncSubmittedQuantum],
  registry: BenchmarkDeviceRegistry
): IO[List[SyncTaskMetric]] = {

  val groupedByDevice: Map[String, List[SyncSubmittedQuantum]] =
    groupSubmitted.groupBy(_.deviceId)

  groupedByDevice.values.toList.flatTraverse { devAssignments =>
    devAssignments.foldLeftM[IO, (Long, List[SyncTaskMetric])]((0L, List.empty[SyncTaskMetric])) {
      case ((accRunMillis, acc), sub) =>
        registry.fakeDevice(sub.deviceId).submitJob(sub.taskId).map { rec =>
          val submittedMillis = rec.submittedAt.atZone(java.time.ZoneId.systemDefault()).toInstant.toEpochMilli
          val startedMillis   = rec.startedAt.atZone(java.time.ZoneId.systemDefault()).toInstant.toEpochMilli
          val finishedMillis  = rec.finishedAt.atZone(java.time.ZoneId.systemDefault()).toInstant.toEpochMilli

          val startMillis  = startedMillis - submittedMillis
          val finishMillis = finishedMillis - submittedMillis

          val m = SyncTaskMetric(
            taskId = sub.taskId,
            deviceId = sub.deviceId,
            startMillis = startMillis,
            finishMillis = finishMillis
          )

          (accRunMillis, acc :+ m)
        }
    }.map(_._2)
  }
}

private def buildGroupMetric(
    groupIndex: Int,
    coherenceBudgetMillis: Long,
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
        startSkewMillis = startSkewMillis,
        finishSkewMillis = finishSkewMillis,
        budgetViolationMillis = budgetViolationMillis,
        survivalProxy = survivalProxy,
        taskMetrics = taskMetrics
    )
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

            Logger[IO].info(
                s"Waiting For All Submitted To Finish. Seen=${mergedSeen.keySet.size}/${expectedIds.size}. Remaining=${remaining.mkString(", ")}"
            ) *>
            (
                if (remaining.isEmpty) mergedSeen.values.toList.pure[IO]
                else Temporal[IO].sleep(pollEvery) *> loop(mergedSeen)
            )
        }

    if (expectedIds.isEmpty) List.empty[SyncSubmittedQuantum].pure[IO]
    else loop(Map.empty)
}

  def runSchedulerSyncBenchmark(
    scheduler: Scheduler[IO],
    groups: List[SyncGroupSpec],
    registry: BenchmarkDeviceRegistry,
    clients: HttpClients[IO],
    compiler: FakeCompiler[IO],
    pollEvery: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.DurationInt(100).millis
  ): IO[SyncBenchmarkRun] =
    for {
      t0 <- monotonicMillis

      syncTaskReqs = groups.map(g => 
        SynronizedQuantumTaskRequest(g.tasks.map(q => 
            NewQuantumTaskRequest(q.circuit, q.qubits, q.shots, q.depth, List(), List(), LocalDateTime.now())), g.coherenceBudgetMillis, false))

      submittedGroups <- groups.zip(syncTaskReqs).zipWithIndex.traverse {
            case ((g, req), idx) =>
                scheduler.submitTask(req).map { ids =>
                SubmittedSyncGroup(
                    groupIndex = idx,
                    coherenceBudgetMillis = g.coherenceBudgetMillis,
                    expectedIds = ids.toSet
                )
                }
            }

      expectedIds = submittedGroups.flatMap(_.expectedIds).toSet

      _ <- Logger[IO].info("Waiting For All Submitted To Finish")
      submitted <- waitUntilAllSubmitted(
        scheduler = scheduler,
        expectedIds = expectedIds,
        pollEvery = pollEvery
      )
      _ <- Logger[IO].info("All Submitted Finished")

      submittedById: Map[TaskId, String] =
        submitted.map(s => s.taskId -> s.deviceId).toMap

      groupMetrics <- submittedGroups.traverse { sg =>
            val groupSubmitted: List[SyncSubmittedQuantum] =
                submitted.filter(s => sg.expectedIds.contains(s.taskId))

            buildSubmittedTaskMetricsForGroup(
                groupSubmitted = groupSubmitted,
                registry = registry
            ).map { taskMetrics =>
                buildGroupMetric(
                groupIndex = sg.groupIndex,
                coherenceBudgetMillis = sg.coherenceBudgetMillis,
                taskMetrics = taskMetrics
                )
            }
     }

      t1 <- monotonicMillis
    } yield SyncBenchmarkRun(
      policyName = "scheduler_sync",
      groupsScheduled = groups.size,
      totalQuantumTasks = groups.map(_.tasks.size).sum,
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

  def runBaselineSyncBenchmark(
    policy: SyncBaselinePolicy,
    groups: List[SyncGroupSpec],
    registry: BenchmarkDeviceRegistry,
    clients: HttpClients[IO],
    compiler: FakeCompiler[IO]
  ): IO[SyncBenchmarkRun] =
    for {
      t0 <- monotonicMillis

      groupMetrics <- groups.zipWithIndex.traverse {
        case (group, idx) =>
          for {
            assignments <- group.tasks.traverse { spec =>
              for {
                tid <- ID.make[IO, TaskId]
                device <- policy match {
                  case SyncBaselinePolicy.IndependentLeastBusy =>
                    chooseIndependentLeastBusyDevice(spec, registry)

                  case SyncBaselinePolicy.IndependentHighestFidelity =>
                    chooseIndependentHighestFidelityDevice(spec, registry, clients, compiler)
                }
              } yield (tid, device.platformId, spec)
            }

            taskMetrics <- buildGroupTaskMetrics(assignments, registry, clients, compiler)
          } yield buildGroupMetric(
            groupIndex = idx,
            coherenceBudgetMillis = group.coherenceBudgetMillis,
            taskMetrics = taskMetrics
          )
      }

      t1 <- monotonicMillis
    } yield SyncBenchmarkRun(
      policyName = policy.name,
      groupsScheduled = groups.size,
      totalQuantumTasks = groups.map(_.tasks.size).sum,
      schedulingWallMillis = t1 - t0,
      groupMetrics = groupMetrics
    )
}