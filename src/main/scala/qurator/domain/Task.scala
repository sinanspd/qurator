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

object Task{
    
    case class TaskId(value: UUID) 

    sealed trait TaskType 
    case object ClassicalTask extends TaskType
    case object QuantumTask extends TaskType

    case class TaskQasm(value: String)

    case class TaskQubits(value: Int)
    case class TaskShots(value: Int)
    case class TaskDepth(value: Int)

    case class Task(
        uuid: TaskId,
        taskType: TaskType,
        qasm: TaskQasm,
        qubits: TaskQubits,
        shots: TaskShots,
        depth: TaskDepth,
        parentTasks: List[TaskId],
        childTasks: List[TaskId],
        createdAt: LocalDateTime
    )

    case class TaskAssignment(
        taskId: TaskId,
        deviceName: String,
        provider: DeviceQueueInformation.DeviceProvider,
        assignedAt: LocalDateTime
    )


}