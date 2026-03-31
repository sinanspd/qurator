package qurator.domain

import ciris._
import ciris.refined._
import com.comcast.ip4s.{ Host, Port }
import eu.timepit.refined.cats._
import eu.timepit.refined.types.net.UserPortNumber
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype
import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import io.estatico.newtype.macros.newtype
import qurator.domain.DeviceQueueInformation._
import qurator.domain.device.Device

object IBM{

    case class IBMConfig(
        instanceId: NonEmptyString,
        apiKey: Secret[NonEmptyString]
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBearerToken(
        access_token: String,
        refresh_token: String,
        token_type: String,
        expires_in: Int,
        expiration: Int,
        scope: String
    )

   @derive(decoder, encoder, eqv, show)
   case class BackendsResponseV2(
    devices: List[IBMBackendDevice]
   )

   @derive(decoder, encoder, eqv, show)
    case class IBMBackendDevice(
        name: String,
        status: IBMBackendDeviceStatus,
        is_simulator: Option[Boolean],
        qubits: Option[Int],
        clops: Option[IBMBackendDeviceClops],
        processor_type: Option[IBMBackendDeviceProcessorType],
        queue_length: Int,
        performance_metrics: Option[IBMBackendDevicePerformanceMetrics],
        wait_time_seconds: Option[IBMBackendDeviceWaitTimeSeconds]
    ){
        def toDevice: Device = 
            Device(
                platformId = name,
                platform = "IBM",
                qubits= qubits.getOrElse(0),
                queueLength = queue_length,
                t1 = 0,
                t2 = 0,
                gateSet = List.empty
            )
    }

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendDeviceStatus(
        name: String,
        reason: Option[String]
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendDeviceClops(
        `type`: String,
        value: Int
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendDeviceProcessorType(
        family: Option[String],
        revision: Option[String],
        segment: Option[String]
    )
        
    @derive(decoder, encoder, eqv, show)
    case class IBMBackendDevicePerformanceMetrics(
        two_q_error_best: Option[IBMBackendDevicePerformanceMetricDetail],
        two_q_error_layered: Option[IBMBackendDevicePerformanceMetricDetail]
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendDevicePerformanceMetricDetail(
        value: Double,
        gate: Option[String],
        unit: Option[String],
        qubits: Option[List[Int]]
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendDeviceWaitTimeSeconds(
        average: Int,
        p50: Int,
        p95: Int
    )

    def toDeviceQueueInformation(l : List[IBMBackendDevice]) =
        l.map(a => DeviceQueueInformationCreate(
            name        = a.name,
            provider    = IBMDevice,
            queueLength = a.queue_length,
            waitTimeAvg = a.wait_time_seconds.map(_.average),
            waitTimep50 = a.wait_time_seconds.map(_.p50),
            waitTimep95 = a.wait_time_seconds.map(_.p95),
            queueType   = NormalQueue
        ) )


    @derive(decoder, encoder, eqv, show)
    case class JobMetricsResponse(
        timestamps: JobTimeStamps,
        bss: JobBSS,
        usage: JobUsage,
        qiskit_version: String,
        estimated_start_time: Option[String],
        estimated_completion_time: Option[String],
        position_in_queue: Option[Int],
        position_in_provider: Option[Int]
    )

    @derive(decoder, encoder, eqv, show)
    case class JobTimeStamps(
        created: String,
        finished: Option[String], 
        running: Option[String]
    )
    
    @derive(decoder, encoder, eqv, show)
    case class JobBSS(
        seconds: Int
    )

    @derive(decoder, encoder, eqv, show)
    case class JobUsage(
        quantum_seconds: Int,
        seconds: Int
    )


    @derive(decoder, encoder, eqv, show)
    case class JobDetailsResponseV2(
        id: String,
        backend: String,
        state: JobState, // "Queued", "Running", "Completed","Cancelled", "Failed"
        status: String, // "Queued", "Running", "Completed","Cancelled", "Failed"
        created: String,
        program: JobProgram,
        runtime: Option[String],
        cost: Int,
        tags: Option[List[String]],
        session_id: Option[String],
        user_id: String,
        `private`: Option[Boolean],
        estimated_running_time_seconds: Option[Double],
        calibration_id: Option[String]
    )   

    @derive(decoder, encoder, eqv, show)
    case class JobState(
        status: String, // "Queued", "Running", "Completed","Cancelled", "Failed"
        reason: Option[String],
        reason_code: Option[Int],
        reason_solution: Option[String]
    )

    @derive(decoder, encoder, eqv, show)
    case class JobProgram(
        id: String
    )

    @derive(decoder, encoder, eqv, show)
    case class CreateJobResponseV2(
        id: String, 
        backend: String, 
        session_id: Option[String],
        `private`: Option[Boolean],
        calibration_id: Option[String]
    )

    @derive(decoder, encoder, eqv, show)
    case class SubmitJobRequestV2(
        program_id: String,
        backend: String,
        runtime: Option[String] = None,
        tags: Option[List[String]] = None,
        log_level: Option[String] = None, //"critical", "error", "warning", "info", "debug"
        cost: Option[Int] = None,
        session_id: Option[String] = None,
        calibration_id: Option[String] = None,
        params:  SamplerV2Input //Either[SamplerV2Input, EstimatorV2Input] // TODO: Don't support Estimators for now.  
    )

    @derive(decoder, encoder, eqv, show)
    case class SamplerV2Input(
        pubs: List[String],
        shots: Option[Int] = None,
        support_qiskit: Option[Boolean] = None,
        version: Int = 2
    )

    @derive(decoder, encoder, eqv, show)
    case class SamplerV2PUB(
        circuit: String,
        shots: Option[Int]
    )    
}