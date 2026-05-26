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
import qurator.domain.circuit._
import qurator.domain.device._
import qurator.domain.IBM.SubmitJobRequestV2
import qurator.domain.Braket.BraketCreateQuantumTaskRequest
import qurator.domain.Azure.AzureJobCreateRequest


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

    sealed trait Task{
        val uuid : TaskId;
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
        quantumResult: Option[QuantumJobResult] = None
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
