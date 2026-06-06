package qurator.dashboard

import cats.effect._
import cats.effect.kernel.Ref
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import qurator.domain.Task._

import java.time.{LocalDateTime, ZoneId}
import java.util.UUID

import com.comcast.ip4s.{Host, Port}
import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.server.Router
import org.http4s.EntityEncoder

final case class SchedulerDashboardConfig(
    host: String = "0.0.0.0",
    port: Int = 8088,
    pathPrefix: String = "/scheduler-dashboard",
    refreshMillis: Int = 1000,
    retainedCompletedTasks: Int = 10000
)

final case class SchedulerDashboardTaskRecord(
    taskId: TaskId,
    kind: String,
    label: String,
    status: String,
    pendingReason: Option[String],
    createdAtMillis: Long,
    parentTaskIds: List[TaskId],
    childTaskIds: List[TaskId],
    qubits: Option[Int],
    depth: Option[Int],
    t1BudgetMillis: Option[Long],
    provider: Option[String],
    deviceId: Option[String],
    jobId: Option[String],
    submittedAtMillis: Option[Long],
    completedAtMillis: Option[Long],
    result: Option[String],
    note: Option[String],
    hiddenFromTaskList: Boolean = false
)

final case class SchedulerDashboardState(
    records: Map[TaskId, SchedulerDashboardTaskRecord],
    completionOrder: Vector[TaskId]
)

object SchedulerDashboardState {
    val empty: SchedulerDashboardState = SchedulerDashboardState(Map.empty, Vector.empty)
}

private[qurator] final case class SchedulerDashboardTaskView(
    taskId: String,
    kind: String,
    label: String,
    status: String,
    pendingReason: Option[String],
    createdAtMillis: Long,
    parentTaskIds: List[String],
    childTaskIds: List[String],
    qubits: Option[Int],
    depth: Option[Int],
    t1BudgetMillis: Option[Long],
    provider: Option[String],
    deviceId: Option[String],
    jobId: Option[String],
    submittedAtMillis: Option[Long],
    completedAtMillis: Option[Long],
    result: Option[String],
    note: Option[String],
    level: Option[Int],
    isSelected: Boolean
)

private[qurator] final case class SchedulerDashboardTasksResponse(
    generatedAtMillis: Long,
    counts: Map[String, Int],
    tasks: List[SchedulerDashboardTaskView]
)

private[qurator] final case class SchedulerDashboardGraphResponse(
    generatedAtMillis: Long,
    selectedTaskId: String,
    nodes: List[SchedulerDashboardTaskView],
    edges: List[(String, String)]
)

object SchedulerDashboard {

    private val zoneId: ZoneId = ZoneId.systemDefault()

    private def normalizePrefix(prefix: String): String = {
        val p = if (prefix.startsWith("/")) prefix else s"/$prefix"
        if (p.endsWith("/")) p.dropRight(1) else p
    }

    def dashboardUrl(config: SchedulerDashboardConfig): String =
        s"http://${config.host}:${config.port}${normalizePrefix(config.pathPrefix)}"

    private def localDateTimeMillis(ldt: LocalDateTime): Long =
        ldt.atZone(zoneId).toInstant.toEpochMilli

    private def taskLabel(task: Task): String = task match {
        case _: ClassicalTask => "Classical"
        case qt: QuantumTask  => s"Quantum • ${qt.qubits.value}q • d=${qt.depth.value}"
        case s: SyncronizedQuantumTaskList => s"Sync Group • ${s.tasks.size} tasks • budget=${s.t1Budget}ms"
    }

    def recordTask(
        state: SchedulerDashboardState,
        task: Task,
        pendingReason: String,
        hiddenFromTaskList: Boolean = false
    ): SchedulerDashboardState = task match {
        case ct: ClassicalTask =>
            val rec = SchedulerDashboardTaskRecord(
                taskId = ct.uuid,
                kind = "classical",
                label = taskLabel(ct),
                status = "pending",
                pendingReason = Some(pendingReason),
                createdAtMillis = localDateTimeMillis(ct.createdAt),
                parentTaskIds = ct.parentTasks,
                childTaskIds = ct.childTasks,
                qubits = None,
                depth = None,
                t1BudgetMillis = None,
                provider = None,
                deviceId = None,
                jobId = None,
                submittedAtMillis = None,
                completedAtMillis = None,
                result = None,
                note = None,
                hiddenFromTaskList = hiddenFromTaskList
            )
            state.copy(records = state.records + (ct.uuid -> rec))

        case qt: QuantumTask =>
            val rec = SchedulerDashboardTaskRecord(
                taskId = qt.uuid,
                kind = "quantum",
                label = taskLabel(qt),
                status = "pending",
                pendingReason = Some(pendingReason),
                createdAtMillis = localDateTimeMillis(qt.createdAt),
                parentTaskIds = qt.parentTasks,
                childTaskIds = qt.childTasks,
                qubits = Some(qt.qubits.value),
                depth = Some(qt.depth.value),
                t1BudgetMillis = None,
                provider = None,
                deviceId = None,
                jobId = None,
                submittedAtMillis = None,
                completedAtMillis = None,
                result = None,
                note = None,
                hiddenFromTaskList = hiddenFromTaskList
            )
            state.copy(records = state.records + (qt.uuid -> rec))

        case _: SyncronizedQuantumTaskList => state
    }

    def markPendingReason(
        state: SchedulerDashboardState,
        taskIds: List[TaskId],
        pendingReason: String
    ): SchedulerDashboardState =
        taskIds.foldLeft(state) { (acc, taskId) =>
            acc.records.get(taskId) match {
                case Some(rec) if rec.status == "pending" =>
                    acc.copy(records = acc.records + (taskId -> rec.copy(pendingReason = Some(pendingReason))))
                case _ => acc
            }
        }

    def markSubmitted(
        state: SchedulerDashboardState,
        taskIds: List[TaskId],
        provider: Option[String],
        deviceId: Option[String],
        jobId: Option[String],
        submittedAtMillis: Long,
        note: Option[String]
    ): SchedulerDashboardState =
        taskIds.foldLeft(state) { (acc, taskId) =>
            acc.records.get(taskId) match {
                case Some(rec) =>
                    val next = rec.copy(
                        status = "submitted",
                        pendingReason = None,
                        provider = provider.orElse(rec.provider),
                        deviceId = deviceId.orElse(rec.deviceId),
                        jobId = jobId.orElse(rec.jobId),
                        submittedAtMillis = Some(submittedAtMillis),
                        note = note.orElse(rec.note)
                    )
                    acc.copy(records = acc.records + (taskId -> next))
                case None => acc
            }
        }

    def markCompleted(
        state: SchedulerDashboardState,
        completion: TaskCompletion,
        retention: Int
    ): SchedulerDashboardState = {
        val updated = state.records.get(completion.taskId) match {
            case Some(rec) =>
                val next = rec.copy(
                    status = "completed",
                    pendingReason = None,
                    provider = completion.provider.orElse(rec.provider),
                    deviceId = completion.deviceId.orElse(rec.deviceId),
                    jobId = completion.jobId.orElse(rec.jobId),
                    completedAtMillis = Some(completion.completedAtMillis),
                    result = Some(completion.result)
                )
                state.copy(
                    records = state.records + (completion.taskId -> next),
                    completionOrder = if (state.completionOrder.contains(completion.taskId)) state.completionOrder else state.completionOrder :+ completion.taskId
                )
            case None => state
        }

        if (retention <= 0 || updated.completionOrder.size <= retention) updated
        else {
            val overflow = updated.completionOrder.size - retention
            val toDrop = updated.completionOrder.take(overflow)
            updated.copy(
                records = updated.records -- toDrop,
                completionOrder = updated.completionOrder.drop(overflow)
            )
        }
    }

    private def recordToView(
        rec: SchedulerDashboardTaskRecord,
        level: Option[Int],
        isSelected: Boolean
    ): SchedulerDashboardTaskView =
        SchedulerDashboardTaskView(
            taskId = rec.taskId.value.toString,
            kind = rec.kind,
            label = rec.label,
            status = rec.status,
            pendingReason = rec.pendingReason,
            createdAtMillis = rec.createdAtMillis,
            parentTaskIds = rec.parentTaskIds.map(_.value.toString),
            childTaskIds = rec.childTaskIds.map(_.value.toString),
            qubits = rec.qubits,
            depth = rec.depth,
            t1BudgetMillis = rec.t1BudgetMillis,
            provider = rec.provider,
            deviceId = rec.deviceId,
            jobId = rec.jobId,
            submittedAtMillis = rec.submittedAtMillis,
            completedAtMillis = rec.completedAtMillis,
            result = rec.result,
            note = rec.note,
            level = level,
            isSelected = isSelected
        )

    def tasksResponse(state: SchedulerDashboardState, nowMillis: Long): SchedulerDashboardTasksResponse = {
        val visible = state.records.values.toList
            .filterNot(_.hiddenFromTaskList)
            .sortBy(rec => (-rec.createdAtMillis, rec.taskId.value.toString))

        val counts = visible.groupBy(_.status).view.mapValues(_.size).toMap

        SchedulerDashboardTasksResponse(
            generatedAtMillis = nowMillis,
            counts = counts,
            tasks = visible.map(rec => recordToView(rec, None, isSelected = false))
        )
    }

    def graphResponse(
        state: SchedulerDashboardState,
        selectedTaskId: TaskId,
        nowMillis: Long
    ): Option[SchedulerDashboardGraphResponse] = {
        val records = state.records
        if (!records.contains(selectedTaskId)) None
        else {
            val reverseParents: Map[TaskId, Set[TaskId]] =
                records.values.toList
                    .flatMap(rec => rec.childTaskIds.map(child => child -> rec.taskId))
                    .groupBy(_._1)
                    .view
                    .mapValues(_.map(_._2).toSet)
                    .toMap
                    .withDefaultValue(Set.empty)

            val reverseChildren: Map[TaskId, Set[TaskId]] =
                records.values.toList
                    .flatMap(rec => rec.parentTaskIds.map(parent => parent -> rec.taskId))
                    .groupBy(_._1)
                    .view
                    .mapValues(_.map(_._2).toSet)
                    .toMap
                    .withDefaultValue(Set.empty)

            def parentIds(id: TaskId): Set[TaskId] =
              records.get(id).map(_.parentTaskIds.toSet).getOrElse(Set.empty[TaskId]) ++ reverseParents(id)

            def childIds(id: TaskId): Set[TaskId] =
              records.get(id).map(_.childTaskIds.toSet).getOrElse(Set.empty[TaskId]) ++ reverseChildren(id)

            def gatherAncestors(frontier: Set[TaskId], seen: Set[TaskId]): Set[TaskId] = {
                if (frontier.isEmpty) seen
                else {
                    val parents = frontier.flatMap(parentIds).filter(records.contains)
                    val fresh = parents -- seen
                    gatherAncestors(fresh, seen ++ fresh)
                }
            }

            def gatherDescendants(frontier: Set[TaskId], seen: Set[TaskId]): Set[TaskId] = {
                if (frontier.isEmpty) seen
                else {
                    val children = frontier.flatMap(childIds).filter(records.contains)
                    val fresh = children -- seen
                    gatherDescendants(fresh, seen ++ fresh)
                }
            }

            def relatedByJob(seed: Set[TaskId]): Set[TaskId] = {
                val jobIds = seed.flatMap(id => records.get(id).flatMap(_.jobId))
                if (jobIds.isEmpty) Set.empty[TaskId]
                else records.collect {
                    case (taskId, rec) if rec.jobId.exists(jobIds.contains) => taskId
                }.toSet
            }

            @annotation.tailrec
            def expandIncluded(seed: Set[TaskId]): Set[TaskId] = {
                val ancestors = gatherAncestors(seed, Set.empty)
                val descendants = gatherDescendants(seed, Set.empty)
                val sameJob = relatedByJob(seed ++ ancestors ++ descendants)
                val next = seed ++ ancestors ++ descendants ++ sameJob
                if (next == seed) seed else expandIncluded(next)
            }

            val includedIds = expandIncluded(Set(selectedTaskId))
            val included = records.filter { case (taskId, _) => includedIds.contains(taskId) }

            val edges = included.keys.toList.flatMap { taskId =>
                childIds(taskId)
                    .filter(included.contains)
                    .toList
                    .map(child => taskId -> child)
            }.distinct

            val parentMap = edges.groupBy(_._2).view.mapValues(_.map(_._1).distinct).toMap.withDefaultValue(Nil)
            val childMap = edges.groupBy(_._1).view.mapValues(_.map(_._2).distinct).toMap.withDefaultValue(Nil)
            val roots = included.keys.filter(id => parentMap(id).isEmpty).toList.sortBy(id => included(id).createdAtMillis)

            val initialLevels = roots.map(_ -> 0).toMap

            @annotation.tailrec
            def levelLoop(
                queue: List[TaskId],
                levels: Map[TaskId, Int]
            ): Map[TaskId, Int] = queue match {
                case Nil => levels
                case h :: t =>
                    val base = levels.getOrElse(h, 0)
                    val (nextLevels, nextQueue) = childMap(h).foldLeft((levels, t)) {
                        case ((accLevels, accQueue), child) =>
                            val childLevel = math.max(accLevels.getOrElse(child, 0), base + 1)
                            (accLevels + (child -> childLevel), if (accQueue.contains(child)) accQueue else accQueue :+ child)
                    }
                    levelLoop(nextQueue, nextLevels)
            }

            val levels = levelLoop(roots, initialLevels).withDefaultValue(0)

            val nodeViews = included.values.toList
                .sortBy(rec => (levels(rec.taskId), rec.createdAtMillis, rec.taskId.value.toString))
                .map(rec => recordToView(rec, Some(levels(rec.taskId)), rec.taskId == selectedTaskId))

            Some(
                SchedulerDashboardGraphResponse(
                    generatedAtMillis = nowMillis,
                    selectedTaskId = selectedTaskId.value.toString,
                    nodes = nodeViews,
                    edges = edges.map { case (from, to) => (from.value.toString, to.value.toString) }
                )
            )
        }
    }

    private implicit val taskViewEncoder: Encoder[SchedulerDashboardTaskView] = Encoder.instance { v =>
        io.circe.Json.obj(
            "taskId" -> v.taskId.asJson,
            "kind" -> v.kind.asJson,
            "label" -> v.label.asJson,
            "status" -> v.status.asJson,
            "pendingReason" -> v.pendingReason.asJson,
            "createdAtMillis" -> v.createdAtMillis.asJson,
            "parentTaskIds" -> v.parentTaskIds.asJson,
            "childTaskIds" -> v.childTaskIds.asJson,
            "qubits" -> v.qubits.asJson,
            "depth" -> v.depth.asJson,
            "t1BudgetMillis" -> v.t1BudgetMillis.asJson,
            "provider" -> v.provider.asJson,
            "deviceId" -> v.deviceId.asJson,
            "jobId" -> v.jobId.asJson,
            "submittedAtMillis" -> v.submittedAtMillis.asJson,
            "completedAtMillis" -> v.completedAtMillis.asJson,
            "result" -> v.result.asJson,
            "note" -> v.note.asJson,
            "level" -> v.level.asJson,
            "isSelected" -> v.isSelected.asJson
        )
    }

    private implicit val tasksResponseEncoder: Encoder[SchedulerDashboardTasksResponse] = Encoder.forProduct3(
        "generatedAtMillis",
        "counts",
        "tasks"
    )(r => (r.generatedAtMillis, r.counts, r.tasks))

    private implicit val graphResponseEncoder: Encoder[SchedulerDashboardGraphResponse] = Encoder.forProduct4(
        "generatedAtMillis",
        "selectedTaskId",
        "nodes",
        "edges"
    )(r => (r.generatedAtMillis, r.selectedTaskId, r.nodes, r.edges))


    def resource[F[_]: Async: Logger](
        config: SchedulerDashboardConfig,
        dashboardState: Ref[F, SchedulerDashboardState]
    ): Resource[F, Unit] = {
        val mountPrefix = normalizePrefix(config.pathPrefix)

        def htmlBody: String =
            SchedulerDashboardPage.html(mountPrefix, config.refreshMillis)

        def decodeTaskId(raw: String): Option[TaskId] =
            Either.catchNonFatal(UUID.fromString(raw)).toOption.map(TaskId(_))

        val dsl = new Http4sDsl[F] {}
        import dsl._

        val htmlContentType = `Content-Type`(MediaType.text.html, Charset.`UTF-8`)

        val routes: HttpRoutes[F] = HttpRoutes.of[F] {
            case GET -> Root =>
                Response[F](Status.Ok)
                  .withEntity(htmlBody)(EntityEncoder.stringEncoder[F])
                  .withContentType(htmlContentType)
                  .pure[F]

            case GET -> Root / "index.html" =>
                Response[F](Status.Ok)
                  .withEntity(htmlBody)(EntityEncoder.stringEncoder[F])
                  .withContentType(htmlContentType)
                  .pure[F]

            case GET -> Root / "api" / "tasks" =>
                Clock[F].realTime.map(_.toMillis).flatMap { now =>
                    dashboardState.get.flatMap { state =>
                        Ok(tasksResponse(state, now).asJson)
                    }
                }

            case GET -> Root / "api" / "tasks" / rawTaskId / "graph" =>
                decodeTaskId(rawTaskId) match {
                    case None =>
                        BadRequest(Json.obj("error" -> "Invalid task id".asJson))

                    case Some(taskId) =>
                        Clock[F].realTime.map(_.toMillis).flatMap { now =>
                            dashboardState.get.flatMap { state =>
                                graphResponse(state, taskId, now) match {
                                    case Some(response) => Ok(response.asJson)
                                    case None           => NotFound(Json.obj("error" -> "Task not found".asJson))
                                }
                            }
                        }
                }
        }

        val httpApp: HttpApp[F] =
            Router(mountPrefix -> routes).orNotFound

        for {
            host <- Resource.eval(
                Async[F].fromOption(
                    Host.fromString(config.host),
                    new IllegalArgumentException(s"Invalid dashboard host: ${config.host}")
                )
            )
            port <- Resource.eval(
                Async[F].fromOption(
                    Port.fromInt(config.port),
                    new IllegalArgumentException(s"Invalid dashboard port: ${config.port}")
                )
            )
            _ <- EmberServerBuilder
                .default[F]
                .withHost(host)
                .withPort(port)
                .withHttpApp(httpApp)
                .build
            _ <- Resource.eval(Logger[F].info(s"Scheduler dashboard started at ${dashboardUrl(config)}"))
            //_ <- Resource.make(Async[F].unit)(_ => Logger[F].info("Scheduler dashboard stopped"))
        } yield ()
    }

}

private[qurator] object SchedulerDashboardPage {
    def html(prefix: String, refreshMillis: Int): String =
        s"""<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Qurator Dashboard</title>
  <style>
    :root {
      --bg: #0b1220;
      --panel: #111827;
      --panel-2: #162033;
      --text: #e5e7eb;
      --muted: #9ca3af;
      --border: #253046;
      --pending: #eab308;
      --submitted: #38bdf8;
      --completed: #22c55e;
      --accent: #8b5cf6;
      --danger: #ef4444;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, sans-serif;
      background: var(--bg);
      color: var(--text);
      height: 100vh;
      overflow: hidden;
    }
    .layout {
      display: grid;
      grid-template-columns: 380px 1fr;
      height: 100vh;
    }
    .sidebar {
      background: var(--panel);
      border-right: 1px solid var(--border);
      display: flex;
      flex-direction: column;
      min-height: 0;
    }
    .sidebar-header, .main-header {
      padding: 18px 20px;
      border-bottom: 1px solid var(--border);
    }
    .title { font-size: 20px; font-weight: 700; }
    .subtitle { color: var(--muted); font-size: 13px; margin-top: 6px; }
    .controls {
      padding: 12px 20px;
      border-bottom: 1px solid var(--border);
      display: grid;
      gap: 10px;
    }
    .search {
      width: 100%;
      border: 1px solid var(--border);
      border-radius: 10px;
      background: #0f172a;
      color: var(--text);
      padding: 10px 12px;
      outline: none;
    }
    .counts { display: flex; gap: 8px; flex-wrap: wrap; }
    .pill {
      border: 1px solid var(--border);
      background: #0f172a;
      color: var(--text);
      border-radius: 999px;
      padding: 4px 10px;
      font-size: 12px;
    }
    .task-list {
      overflow: auto;
      padding: 10px 14px 18px;
      display: grid;
      gap: 10px;
    }
    .task-row {
      border: 1px solid var(--border);
      background: var(--panel-2);
      border-radius: 14px;
      padding: 12px;
      cursor: pointer;
      transition: border-color .15s ease, transform .15s ease;
    }
    .task-row:hover { border-color: #4b5563; transform: translateY(-1px); }
    .task-row.selected { border-color: var(--accent); box-shadow: 0 0 0 1px rgba(139,92,246,.35) inset; }
    .row-top { display: flex; justify-content: space-between; gap: 8px; align-items: flex-start; }
    .task-label { font-weight: 700; font-size: 14px; }
    .task-id { color: var(--muted); font-size: 11px; margin-top: 4px; }
    .status { font-size: 11px; padding: 3px 8px; border-radius: 999px; font-weight: 700; text-transform: uppercase; }
    .status.pending { background: rgba(234,179,8,.12); color: var(--pending); }
    .status.submitted { background: rgba(56,189,248,.12); color: var(--submitted); }
    .status.completed { background: rgba(34,197,94,.12); color: var(--completed); }
    .meta { margin-top: 10px; color: var(--muted); font-size: 12px; line-height: 1.45; }
    .main {
      display: grid;
      grid-template-rows: auto auto 1fr;
      min-width: 0;
      min-height: 0;
    }
    .detail-panel {
      padding: 14px 20px;
      border-bottom: 1px solid var(--border);
      background: rgba(17,24,39,.72);
    }
    .detail-grid {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 12px;
      margin-top: 12px;
    }
    .detail-card {
      border: 1px solid var(--border);
      background: var(--panel);
      border-radius: 12px;
      padding: 12px;
    }
    .detail-card .k { color: var(--muted); font-size: 11px; text-transform: uppercase; letter-spacing: .05em; }
    .detail-card .v { margin-top: 6px; font-size: 13px; word-break: break-word; }
    .graph-wrap {
      overflow: auto;
      position: relative;
      background: radial-gradient(circle at top left, rgba(139,92,246,.08), transparent 34%), var(--bg);
    }
    .graph-canvas {
      position: relative;
      min-width: 100%;
      min-height: 100%;
    }
    svg.edges {
      position: absolute;
      inset: 0;
      overflow: visible;
      pointer-events: none;
    }
    .edge {
      stroke: #334155;
      stroke-width: 2;
      fill: none;
      marker-end: url(#arrowhead);
    }
    .edge.merge-edge {
      stroke: #8b5cf6;
      stroke-dasharray: 6 4;
    }
    .node {
      position: absolute;
      width: 240px;
      min-height: 100px;
      border: 1px solid var(--border);
      border-left-width: 6px;
      border-radius: 16px;
      background: var(--panel);
      padding: 12px;
      box-shadow: 0 10px 30px rgba(0,0,0,.18);
    }
    .node.pending { border-left-color: var(--pending); }
    .node.submitted { border-left-color: var(--submitted); }
    .node.completed { border-left-color: var(--completed); }
    .node.selected { box-shadow: 0 0 0 2px rgba(139,92,246,.5), 0 10px 30px rgba(0,0,0,.24); }
    .node.merge {
      width: 300px;
      background: linear-gradient(180deg, rgba(139,92,246,.10), rgba(17,24,39,1));
      border-color: rgba(139,92,246,.55);
      border-left-color: var(--accent);
      box-shadow: 0 0 0 1px rgba(139,92,246,.25) inset, 0 10px 30px rgba(0,0,0,.22);
    }
    .node .label { font-weight: 700; font-size: 13px; }
    .node .small { color: var(--muted); font-size: 11px; margin-top: 5px; line-height: 1.35; }
    .merge-badge {
      display: inline-block;
      margin-top: 8px;
      padding: 3px 8px;
      border-radius: 999px;
      font-size: 10px;
      font-weight: 700;
      letter-spacing: .03em;
      text-transform: uppercase;
      color: #c4b5fd;
      background: rgba(139,92,246,.18);
      border: 1px solid rgba(139,92,246,.35);
    }
    .merge-members {
      margin-top: 10px;
      display: grid;
      gap: 6px;
    }
    .merge-member {
      border: 1px solid rgba(139,92,246,.18);
      border-radius: 10px;
      padding: 7px 8px;
      background: rgba(15,23,42,.55);
    }
    .merge-member.selected-member {
      border-color: rgba(139,92,246,.65);
      box-shadow: 0 0 0 1px rgba(139,92,246,.25) inset;
    }
    .merge-member .mm-title {
      font-size: 11px;
      font-weight: 700;
    }
    .merge-member .mm-meta {
      color: var(--muted);
      font-size: 10px;
      margin-top: 3px;
      line-height: 1.35;
    }
    .empty {
      padding: 30px 20px;
      color: var(--muted);
      font-size: 14px;
    }
    @media (max-width: 1100px) {
      .layout { grid-template-columns: 320px 1fr; }
      .detail-grid { grid-template-columns: repeat(2, minmax(0,1fr)); }
    }
  </style>
</head>
<body>
  <div class="layout">
    <aside class="sidebar">
      <div class="sidebar-header">
        <div class="title">Qurator Dashboard</div>
        <div class="subtitle">Tasks, dependency graph, and execution state.</div>
      </div>
      <div class="controls">
        <input id="search" class="search" type="text" placeholder="Search by task id, kind, device..." />
        <div id="counts" class="counts"></div>
      </div>
      <div id="task-list" class="task-list"></div>
    </aside>
    <main class="main">
      <div class="main-header">
        <div class="title">Task Dependency View</div>
        <div class="subtitle" id="header-subtitle">Select a task from the left to inspect its full dependency tree, including ancestors, descendants, and merged execution groups.</div>
      </div>
      <div id="detail-panel" class="detail-panel">
        <div class="subtitle">No task selected.</div>
      </div>
      <div class="graph-wrap">
        <div id="graph-canvas" class="graph-canvas">
          <div class="empty">Select a task to render the dependency/dataflow graph.</div>
        </div>
      </div>
    </main>
  </div>
  <script>
    const prefix = ${prefix.asJson.noSpaces};
    const refreshMillis = ${refreshMillis};
    let selectedTaskId = null;
    let taskCache = [];

    const fmtTs = (ms) => ms ? new Date(ms).toLocaleString() : '—';

    function statusBadge(status) {
      return `<span class="status $${status}">$${status}</span>`;
    }

    function secondaryMeta(task) {
      const bits = [];
      if (task.kind === 'quantum') {
        if (task.qubits != null) bits.push(`$${task.qubits}q`);
        if (task.depth != null) bits.push(`depth $${task.depth}`);
      } else {
        bits.push(task.kind.replace('_', ' '));
      }
      if (task.pendingReason) bits.push(task.pendingReason.replaceAll('_', ' '));
      if (task.deviceId) bits.push(task.deviceId);
      return bits.join(' • ');
    }

    function renderCounts(counts) {
      const order = ['pending', 'submitted', 'completed'];
      const html = order.map(k => `<span class="pill">$${k}: $${counts[k] || 0}</span>`).join('');
      document.getElementById('counts').innerHTML = html;
    }

    function renderTaskList(tasks) {
      taskCache = tasks;
      const q = document.getElementById('search').value.trim().toLowerCase();
      const filtered = !q ? tasks : tasks.filter(task => {
        const blob = [
          task.taskId,
          task.kind,
          task.label,
          task.status,
          task.deviceId || '',
          task.provider || '',
          task.jobId || ''
        ].join(' ').toLowerCase();
        return blob.includes(q);
      });

      const list = document.getElementById('task-list');
      if (!filtered.length) {
        list.innerHTML = '<div class="empty">No matching tasks.</div>';
        return;
      }

      list.innerHTML = filtered.map(task => `
        <div class="task-row $${selectedTaskId === task.taskId ? 'selected' : ''}" data-task-id="$${task.taskId}">
          <div class="row-top">
            <div>
              <div class="task-label">$${task.label}</div>
              <div class="task-id">$${task.taskId}</div>
            </div>
            $${statusBadge(task.status)}
          </div>
          <div class="meta">
            <div>$${secondaryMeta(task)}</div>
            $${task.note ? `<div>$${task.note}</div>` : ''}
            <div>Created: $${fmtTs(task.createdAtMillis)}</div>
            $${task.submittedAtMillis ? `<div>Submitted: $${fmtTs(task.submittedAtMillis)}</div>` : ''}
            $${task.completedAtMillis ? `<div>Completed: $${fmtTs(task.completedAtMillis)}</div>` : ''}
          </div>
        </div>
      `).join('');

      list.querySelectorAll('.task-row').forEach(row => {
        row.addEventListener('click', () => {
          selectedTaskId = row.dataset.taskId;
          renderTaskList(taskCache);
          loadGraph(selectedTaskId);
        });
      });
    }

    function renderDetail(task) {
      const panel = document.getElementById('detail-panel');
      if (!task) {
        panel.innerHTML = '<div class="subtitle">No task selected.</div>';
        return;
      }
      panel.innerHTML = `
        <div class="title" style="font-size:18px;">$${task.label}</div>
        <div class="subtitle">$${task.taskId}</div>
        <div class="detail-grid">
          <div class="detail-card"><div class="k">Status</div><div class="v">$${task.status}$${task.pendingReason ? ' • ' + task.pendingReason.replaceAll('_', ' ') : ''}</div></div>
          <div class="detail-card"><div class="k">Device</div><div class="v">$${task.deviceId || '—'}</div></div>
          <div class="detail-card"><div class="k">Job ID</div><div class="v">$${task.jobId || '—'}</div></div>
          <div class="detail-card"><div class="k">Timing</div><div class="v">Created $${fmtTs(task.createdAtMillis)}<br/>Submitted $${fmtTs(task.submittedAtMillis)}<br/>Completed $${fmtTs(task.completedAtMillis)}</div></div>
          <div class="detail-card"><div class="k">Execution</div><div class="v">$${task.note || '—'}</div></div>
        </div>
      `;
    }

    function buildDisplayGraph(graph) {
      const nodesById = Object.fromEntries((graph.nodes || []).map(node => [node.taskId, { ...node }]));
      const baseEdges = (graph.edges || []).map(([from, to]) => ({ from, to }));
      const grouped = {};

      Object.values(nodesById).forEach(node => {
        if (node.jobId) {
          grouped[node.jobId] = grouped[node.jobId] || [];
          grouped[node.jobId].push(node);
        }
      });

      const mergeGroups = Object.entries(grouped)
        .filter(([, members]) => members.length > 1)
        .map(([jobId, members]) => ({
          groupId: `merge::$${jobId}`,
          jobId,
          members: members.sort((a, b) => (a.createdAtMillis || 0) - (b.createdAtMillis || 0))
        }));

      const memberToGroup = {};
      mergeGroups.forEach(group => group.members.forEach(member => {
        memberToGroup[member.taskId] = group.groupId;
      }));

      const displayNodes = {};
      Object.values(nodesById).forEach(node => {
        if (!memberToGroup[node.taskId]) {
          displayNodes[node.taskId] = { ...node, displayKind: 'task' };
        }
      });

      mergeGroups.forEach(group => {
        const rep = group.members[0];
        const submittedAtMillis = group.members.map(m => m.submittedAtMillis).filter(Boolean).sort((a, b) => a - b)[0] || null;
        const completedAtMillis = group.members.map(m => m.completedAtMillis).filter(Boolean).sort((a, b) => a - b).slice(-1)[0] || null;
        const anySelected = group.members.some(m => m.isSelected);
        const status = group.members.some(m => m.status === 'submitted')
          ? 'submitted'
          : (group.members.every(m => m.status === 'completed') ? 'completed' : 'pending');

        displayNodes[group.groupId] = {
          taskId: group.groupId,
          kind: 'merged_quantum',
          displayKind: 'merge',
          label: `Merged execution • $${group.members.length} tasks`,
          status,
          pendingReason: null,
          createdAtMillis: rep.createdAtMillis,
          parentTaskIds: [],
          childTaskIds: [],
          qubits: group.members.reduce((acc, m) => acc + (m.qubits || 0), 0),
          depth: Math.max(...group.members.map(m => m.depth || 0)),
          t1BudgetMillis: null,
          provider: rep.provider,
          deviceId: rep.deviceId,
          jobId: rep.jobId,
          submittedAtMillis,
          completedAtMillis,
          result: null,
          note: rep.note || 'Merged physical execution',
          level: rep.level,
          isSelected: anySelected,
          mergeMembers: group.members.map(m => ({
            taskId: m.taskId,
            label: m.label,
            status: m.status,
            qubits: m.qubits,
            depth: m.depth,
            isSelected: m.isSelected
          }))
        };
      });

      const edgeMap = new Map();
      baseEdges.forEach(({ from, to }) => {
        const fromDisplay = memberToGroup[from] || from;
        const toDisplay = memberToGroup[to] || to;
        if (!displayNodes[fromDisplay] || !displayNodes[toDisplay] || fromDisplay === toDisplay) return;
        const key = `$${fromDisplay}->$${toDisplay}`;
        edgeMap.set(key, {
          from: fromDisplay,
          to: toDisplay,
          isMergeEdge: String(fromDisplay).startsWith('merge::') || String(toDisplay).startsWith('merge::')
        });
      });

      const edges = Array.from(edgeMap.values());
      return {
        selectedTaskId: graph.selectedTaskId,
        nodes: Object.values(displayNodes),
        edges
      };
    }

    function computeLayout(nodes, edges) {
      const nodesById = Object.fromEntries(nodes.map(node => [node.taskId, node]));
      const indegree = {};
      const outgoing = {};
      const incoming = {};
      nodes.forEach(node => {
        indegree[node.taskId] = 0;
        outgoing[node.taskId] = [];
        incoming[node.taskId] = [];
      });
      edges.forEach(edge => {
        if (!nodesById[edge.from] || !nodesById[edge.to]) return;
        indegree[edge.to] += 1;
        outgoing[edge.from].push(edge.to);
        incoming[edge.to].push(edge.from);
      });

      const roots = nodes
        .filter(node => indegree[node.taskId] === 0)
        .sort((a, b) => (a.createdAtMillis || 0) - (b.createdAtMillis || 0))
        .map(node => node.taskId);

      const queue = [...roots];
      const level = Object.fromEntries(roots.map(id => [id, 0]));
      const seen = new Set(queue);
      while (queue.length) {
        const cur = queue.shift();
        const base = level[cur] || 0;
        (outgoing[cur] || []).forEach(child => {
          level[child] = Math.max(level[child] || 0, base + 1);
          if (!seen.has(child)) {
            seen.add(child);
            queue.push(child);
          }
        });
      }
      nodes.forEach(node => { if (level[node.taskId] == null) level[node.taskId] = 0; });

      const levels = [...new Set(Object.values(level))].sort((a, b) => a - b);
      const columns = {};
      levels.forEach(l => {
        columns[l] = nodes.filter(node => level[node.taskId] === l);
      });

      let orderMap = {};
      levels.forEach(l => {
        columns[l].sort((a, b) => (a.createdAtMillis || 0) - (b.createdAtMillis || 0));
        columns[l].forEach((node, idx) => { orderMap[node.taskId] = idx; });
      });

      for (let iter = 0; iter < 4; iter += 1) {
        levels.forEach(l => {
          columns[l].sort((a, b) => {
            const ap = incoming[a.taskId] || [];
            const bp = incoming[b.taskId] || [];
            const ax = ap.length ? ap.map(id => orderMap[id] ?? 0).reduce((s, x) => s + x, 0) / ap.length : (orderMap[a.taskId] ?? 0);
            const bx = bp.length ? bp.map(id => orderMap[id] ?? 0).reduce((s, x) => s + x, 0) / bp.length : (orderMap[b.taskId] ?? 0);
            return ax - bx || ((a.createdAtMillis || 0) - (b.createdAtMillis || 0));
          });
          columns[l].forEach((node, idx) => { orderMap[node.taskId] = idx; });
        });

        [...levels].reverse().forEach(l => {
          columns[l].sort((a, b) => {
            const ac = outgoing[a.taskId] || [];
            const bc = outgoing[b.taskId] || [];
            const ax = ac.length ? ac.map(id => orderMap[id] ?? 0).reduce((s, x) => s + x, 0) / ac.length : (orderMap[a.taskId] ?? 0);
            const bx = bc.length ? bc.map(id => orderMap[id] ?? 0).reduce((s, x) => s + x, 0) / bc.length : (orderMap[b.taskId] ?? 0);
            return ax - bx || ((a.createdAtMillis || 0) - (b.createdAtMillis || 0));
          });
          columns[l].forEach((node, idx) => { orderMap[node.taskId] = idx; });
        });
      }

      const paddingX = 70;
      const paddingY = 60;
      const colGap = 360;
      const rowGap = 42;
      const positions = {};
      let maxBottom = 0;
      let maxRight = 0;

      levels.forEach(l => {
        let cursorY = paddingY;
        columns[l].forEach(node => {
          const width = node.displayKind === 'merge' ? 300 : 240;
          const height = node.displayKind === 'merge'
            ? Math.max(150, 108 + (node.mergeMembers ? node.mergeMembers.length * 42 : 0))
            : 110;
          positions[node.taskId] = {
            x: paddingX + l * colGap,
            y: cursorY,
            width,
            height
          };
          cursorY += height + rowGap;
          maxBottom = Math.max(maxBottom, cursorY);
          maxRight = Math.max(maxRight, paddingX + l * colGap + width);
        });
      });

      return {
        positions,
        width: maxRight + 100,
        height: maxBottom + 40
      };
    }

    function renderGraph(graph) {
      const canvas = document.getElementById('graph-canvas');
      if (!graph || !graph.nodes || !graph.nodes.length) {
        canvas.innerHTML = '<div class="empty">No graph available for this task.</div>';
        return;
      }

      const displayGraph = buildDisplayGraph(graph);
      const nodesById = Object.fromEntries((graph.nodes || []).map(node => [node.taskId, node]));
      const selectedTask = nodesById[graph.selectedTaskId] || graph.nodes.find(n => n.isSelected) || graph.nodes[0];
      const { positions, width, height } = computeLayout(displayGraph.nodes, displayGraph.edges);

      const edgeHtml = displayGraph.edges.map(edge => {
        const a = positions[edge.from];
        const b = positions[edge.to];
        if (!a || !b) return '';
        const x1 = a.x + a.width;
        const y1 = a.y + a.height / 2;
        const x2 = b.x;
        const y2 = b.y + b.height / 2;
        const bend = Math.max(40, (x2 - x1) / 2);
        return `<path class="edge $${edge.isMergeEdge ? 'merge-edge' : ''}" d="M $${x1} $${y1} C $${x1 + bend} $${y1}, $${x2 - bend} $${y2}, $${x2} $${y2}" />`;
      }).join('');

      const nodeHtml = displayGraph.nodes.map(node => {
        const pos = positions[node.taskId];
        const mergeMembersHtml = node.displayKind === 'merge'
          ? `<div class="merge-badge">merged physical execution</div>
             <div class="merge-members">$${(node.mergeMembers || []).map(member => `
               <div class="merge-member $${member.isSelected ? 'selected-member' : ''}">
                 <div class="mm-title">$${member.label}</div>
                 <div class="mm-meta">$${member.taskId}<br/>$${member.qubits != null ? member.qubits + 'q' : ''}$${member.depth != null ? ' • depth ' + member.depth : ''} • $${member.status}</div>
               </div>`).join('')}</div>`
          : '';

        return `
          <div class="node $${node.status} $${node.isSelected ? 'selected' : ''} $${node.displayKind === 'merge' ? 'merge' : ''}" style="left:$${pos.x}px; top:$${pos.y}px; width:$${pos.width}px; min-height:$${pos.height}px;">
            <div class="label">$${node.label}</div>
            <div class="small">$${node.taskId}</div>
            <div class="small">$${secondaryMeta(node)}</div>
            <div class="small">$${node.deviceId ? 'Device: ' + node.deviceId : ''}</div>
            <div class="small">$${node.completedAtMillis ? 'Completed: ' + fmtTs(node.completedAtMillis) : (node.submittedAtMillis ? 'Submitted: ' + fmtTs(node.submittedAtMillis) : '')}</div>
            $${mergeMembersHtml}
          </div>
        `;
      }).join('');

      canvas.style.width = width + 'px';
      canvas.style.height = height + 'px';
      canvas.innerHTML = `
        <svg class="edges" width="$${width}" height="$${height}" viewBox="0 0 $${width} $${height}">
          <defs>
            <marker id="arrowhead" markerWidth="10" markerHeight="8" refX="9" refY="4" orient="auto" markerUnits="strokeWidth">
              <path d="M 0 0 L 10 4 L 0 8 z" fill="#475569"></path>
            </marker>
          </defs>
          $${edgeHtml}
        </svg>
        $${nodeHtml}
      `;

      renderDetail(selectedTask);
      document.getElementById('header-subtitle').textContent = 'Showing the full dependency tree: all reachable ancestors up to root, the selected task, all descendants, and merged execution groups.';
    }

    async function loadTasks() {
      const resp = await fetch(`$${prefix}/api/tasks`);
      const payload = await resp.json();
      renderCounts(payload.counts || {});
      renderTaskList(payload.tasks || []);
      if (!selectedTaskId && payload.tasks && payload.tasks.length) {
        selectedTaskId = payload.tasks[0].taskId;
        renderTaskList(payload.tasks);
        await loadGraph(selectedTaskId);
      }
    }

    async function loadGraph(taskId) {
      if (!taskId) return;
      const resp = await fetch(`$${prefix}/api/tasks/$${taskId}/graph`);
      const payload = await resp.json();
      if (payload.error) {
        renderGraph(null);
        return;
      }
      renderGraph(payload);
    }

    document.getElementById('search').addEventListener('input', () => renderTaskList(taskCache));

    loadTasks();
    setInterval(async () => {
      await loadTasks();
      if (selectedTaskId) await loadGraph(selectedTaskId);
    }, refreshMillis);
  </script>
</body>
</html>"""
}
