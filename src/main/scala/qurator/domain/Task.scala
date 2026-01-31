package qurator.domain

import ciris._

import qurator.optics.uuid
import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive

import java.util.UUID
import java.time.LocalDateTime
import ciris._
import ciris.refined._
import com.comcast.ip4s.{ Host, Port }
import eu.timepit.refined.cats._
import eu.timepit.refined.types.net.UserPortNumber
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype
import com.sinanspd.qure.circuit.Circuit

object Task{

    @derive(decoder, encoder, eqv, show, uuid)
    @newtype
    case class TaskId(value: UUID) 

    @derive(decoder, encoder, eqv, show, uuid)
    @newtype
    case class SyncronizedQuantumTaskId(value: UUID)

    case class TaskQubits(value: Int)
    case class TaskShots(value: Int)
    case class TaskDepth(value: Int)

    sealed trait Task
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
        uuid: SyncronizedQuantumTaskId,
        tasks: List[QuantumTask], 
        t1Budged: Long, 
        createdAt: LocalDateTime
    ) extends Task


    sealed trait TaskRequest 

    case class SynronizedQuantumTaskRequest(l: List[NewQuantumTaskRequest], t1Budget: Long) extends TaskRequest

    case class NewClassicalTaskRequest(
       program: Any, //TODO: Fix This 
       parentTasks: List[TaskId],
       childTasks: List[TaskId],
       createdAt: LocalDateTime
    )

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


}