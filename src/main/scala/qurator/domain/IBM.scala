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
   ) extends ProviderDeviceList[IBMBackendDevice]

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
    ) extends ProviderDeviceSummary with ProviderDeviceDetails {
        def platformId: String =
            name

        def isAvailable: Boolean =
            status.name == "online"

        def toDevice: Device = 
            Device(
                platformId = name,
                platform = "IBM",
                qubits= qubits.getOrElse(0),
                queueLength = queue_length,
                t1 = 0,
                t2 = 0,
                gateSet = List.empty // TODO, need fixing
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

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendPropertiesResponse(
        backend_name: String,
        backend_version: Option[String],
        last_update_date: Option[String],
        qubits: List[List[IBMBackendNamedValue]],
        gates: List[IBMBackendGateProperties],
        general: Option[List[IBMBackendNamedValue]]
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendNamedValue(
        date: Option[String],
        name: String,
        unit: Option[String],
        value: Double
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendGateProperties(
        gate: String,
        name: String,
        parameters: List[IBMBackendNamedValue],
        qubits: List[Int]
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendConfigurationResponse(
        backend_name: String,
        backend_version: Option[String],
        basis_gates: Option[List[String]],
        coupling_map: Option[List[List[Int]]],
        gates: Option[List[IBMBackendConfigurationGate]],
        n_qubits: Option[Int]
    )

    @derive(decoder, encoder, eqv, show)
    case class IBMBackendConfigurationGate(
        name: String,
        coupling_map: List[List[Int]]
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
        finished: Option[String], // Do these Option just to be safe, what happens if the job fails?? 
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
        //params: Map[String, String], //TODO:  Don't Parse this for now 
        //program: Map[String, String], //TODO:  Don't Parse this for now 
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
    ) extends ProviderTaskStatus {
        def taskStatus: String =
            status
    }

//     "usage": {
//       "title": "Usage",
//       "description": "usage metrics",
//       "type": "object",
//       "properties": {
//         "seconds": {
//           "type": "number",
//           "description": "Number of seconds of Qiskit Runtime usage including quantum compute and near-time classical pre- and post-processing"
//         }
//       },
//       "required": [
//         "seconds"
//       ]
//     },

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
    ) extends ProviderTaskSubmission {
        def jobId: String =
            id
    }

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
        pubs: List[String],//List[SamplerV2PUB],
        //options: Option[SamplerV2Options], // TODO: Not supported yet 
        shots: Option[Int] = None,
        support_qiskit: Option[Boolean] = None,
        version: Int = 2
    )

    @derive(decoder, encoder, eqv, show)
    case class SamplerV2PUB(
        circuit: String,
        // parameters: Option[Map[String, Double]], // TODO: Not supported yet 
        shots: Option[Int]
    )

    // case class SamplerV2Options(
    //     default_shots: Option[Int],
    //     dynamical_decoupling: Option[DynamicalDecouplingOptions],
    //     execution: Option[SamplerV2ExecutionOptions],
    //     twirling: Option[TwirlingOptions],
    //     simulator: Option[SimulatorOptions],
    //     experimental: Option[Map[String, String]]
    // )

    // case class DynamicalDecouplingOptions(
    //     enable: Option[Boolean],
    //     sequence_type: Option[String],
    //     extra_slack_distribution: Option[String],
    //     scheduling_method: Option[String], //TODO: This might become relevant 
    //     skip_reset_qubits: Option[Boolean]
    // )

    // case class SamplerV2ExecutionOptions(
    //     init_qubits: Option[Boolean],
    //     rep_delay: Option[Double], //TODO: This might become relevant 
    //     meas_type: Option[String]
    // )

    // case class TwirlingOptions(
    //     enable_gates: Option[Boolean],
    //     enable_measure: Option[Boolean],
    //     num_randomizations: Option[Either[Int, String]],
    //     shots_per_randomization: Option[Either[Int, String]],
    //     strategy: Option[String]
    // )
    
    // case class SimulatorOptions(
    //     noise_model: Option[Map[String, String]],
    //     seed_simulator: Option[Int],
    //     coupling_map: Option[List[Int]],
    //     basis_gates: Option[List[Option[String]]]
    // )

    // case class EstimatorV2Input(
    //     pubs: List[EstimatorV2PUB],
    //     options: Option[EstimatorV2Options],
    //     shots: Option[Int],
    //     support_qiskit: Option[Boolean],
    //     version: Int = 2
    // )

    // case class EstimatorV2PUB(
    //     circuit: String,
    //     observables: Either[String, List[String]],
    //     parameters: Option[Map[String, Double]],
    //     precision: Option[Double]
    // )

    // case class EstimatorV2Options(
    //     seed_estimator: Option[Int],
    //     default_precision: Option[Double],
    //     default_shots: Option[Int],
    //     dynamical_decoupling: Option[DynamicalDecouplingOptions],
    //     resilience: Option[ResilienceOptions],
    //     execution: Option[EstimatorV2ExecutionOptions]
    // )

    // case class ResilienceOptions(
    //     measure_mitigation: Option[Boolean],
    //     measure_noise_learning: Option[MeasureNoiseLearningOptions],
    //     zne_mitigation: Option[Boolean],
    //     zne: Option[ZNEOptions],
    //     pec_mitigation: Option[Boolean],
    //     pec: Option[PECOptions],
    //     layer_noise_learning: Option[LayerNoiseLearningOptions]
    // )

    // case class LayerNoiseLearningOptions(

    // )

    // case class EstimatorV2ExecutionOptions(
    //     init_qubits: Option[Boolean],
    //     rep_delay: Option[Double],
    //     meas_type: Option[String]
    // )

    // case class MeasureNoiseLearningOptions(
    //     num_randomizations: Option[Int],
    //     shots_per_randomization: Option[Either[Int, String]]
    // )

    // case class ZNEOptions(
    //     noise_factors: Option[List[Double]],
    //     extrapolator: Option[Either[List[String], String]],
    //     amplifier: Option[String],
    //     extrapolated_noise_factors: Option[List[Double]]
    // )

    // case class PECOptions(
    //     max_overhead: Option[Either[Double, String]],
    //     noise_gain: Option[Either[Double, String]]
    // )
    
}
