package qurator.domain

import ciris.*
import qurator.optics.{IsUUID, uuid}
import io.circe.generic.auto.*
import cats.derived.*
import cats.{Eq, Show}
import java.util.UUID
import java.time.LocalDateTime
import ciris.*
import ciris.refined.*
import com.comcast.ip4s.{Host, Port}
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.net.UserPortNumber
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import qurator.domain.circuit.*
import qurator.domain.device.*
import qurator.domain.IBM.SubmitJobRequestV2
import qurator.domain.Braket.BraketCreateQuantumTaskRequest
import qurator.domain.Azure.AzureJobCreateRequest

object Task{

  opaque type TaskId = UUID

  opaque type SyncronizedQuantumTaskId = UUID

  object TaskId {
    def apply(value: UUID): TaskId = value

    extension (id: TaskId) {
      def value: UUID = id
    }

    given Eq[TaskId] = Eq.fromUniversalEquals
    given Show[TaskId] = Show.fromToString
    given IsUUID[TaskId] = IsUUID.opaqueUUID[TaskId]
  }

  object SyncronizedQuantumTaskId {
    def apply(value: UUID): SyncronizedQuantumTaskId = value

    extension (id: SyncronizedQuantumTaskId) {
      def value: UUID = id
    }

    given Eq[SyncronizedQuantumTaskId] = summon[Eq[UUID]]

    given Show[SyncronizedQuantumTaskId] = summon[Show[UUID]]

    given IsUUID[SyncronizedQuantumTaskId] = IsUUID.opaqueUUID[SyncronizedQuantumTaskId]
  }

    case class TaskQubits(value: Int)
    case class TaskShots(value: Int)
    case class TaskDepth(value: Int)

    sealed trait Task{
        val uuid : TaskId
    }
    
    case class ClassicalTask(
       uuid: TaskId,
       program: Any, //TODO: Fix This 
       parentTasks: List[TaskId],
       childTasks: List[TaskId],
       createdAt: LocalDateTime
    ) extends Task

    case class QuantumTask(
        uuid: TaskId,
        circuit: Circuit,
        qubits: TaskQubits,
        shots: TaskShots,
        depth: TaskDepth,
        parentTasks: List[TaskId],
        childTasks: List[TaskId],
        createdAt: LocalDateTime
    ) extends Task

    case class SyncronizedQuantumTaskList(
        uuid: TaskId,
        tasks: List[QuantumTask], 
        t1Budget: Long, 
        createdAt: LocalDateTime
    ) extends Task

    case class TaskCompletion(
        taskId: TaskId,
        result: String,
        completedAtMillis: Long,
        provider: Option[String] = None,
        deviceId: Option[String] = None,
        jobId: Option[String] = None,
        executedCircuit: Option[Circuit] = None,
        quantumResult: Option[QuantumResult] = None
    )

    sealed trait TaskRequest 

    case class SynronizedQuantumTaskRequest(l: List[NewQuantumTaskRequest], t1Budget: Long, cut: Boolean = false) extends TaskRequest

    case class NewClassicalTaskRequest(
       program: Any, //TODO: Fix This 
       parentTasks: List[TaskId],
       childTasks: List[TaskId],
       createdAt: LocalDateTime
    ) extends TaskRequest

    case class NewQuantumTaskRequest(
        circuit: Circuit,
        qubits: TaskQubits,
        shots: TaskShots,
        depth: TaskDepth,
        parentTasks: List[TaskId],
        childTasks: List[TaskId],
        createdAt: LocalDateTime
    ) extends TaskRequest

    case class TaskAssignment(
        taskId: TaskId,
        deviceName: String,
        provider: DeviceQueueInformation.DeviceProvider,
        assignedAt: LocalDateTime
    )

    case class CandidateDevice(
        device: Device,
        fidelity: Double,
        queueMillis: Long,
        runMillis: Long
    )

    case class SynchronizedPlan(
        assignments: Map[Device, List[QuantumTask]]
    )


    implicit class TaskTransformations(t: Task) { 
        def toIBM : SubmitJobRequestV2 = ???
        def toBraket : BraketCreateQuantumTaskRequest = ???
        def toAzure : AzureJobCreateRequest = ???
    }


}
