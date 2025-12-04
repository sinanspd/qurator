package qurator.domain

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


object DeviceQueueInformation{

    @derive(decoder, encoder, eqv, show, uuid)
    @newtype
    case class DeviceQueueInformationId(
        value: UUID
    )

    @derive(decoder, encoder, eqv)
    case class DeviceQueueInformation(
        uuid: DeviceQueueInformationId,
        name: String, //ARN if available otherwise name 
        provider: DeviceProvider,
        queueLength: Int,
        waitTimeAvg: Option[Int],
        waitTimep50: Option[Int],
        waitTimep95: Option[Int],
        queueType: QueueType,
        createdAt: LocalDateTime
    )

    @derive(decoder, encoder, eqv)
    case class DeviceQueueInformationCreate(
        name: String, //ARN if available otherwise name 
        provider: DeviceProvider,
        queueLength: Int,
        waitTimeAvg: Option[Int],
        waitTimep50: Option[Int],
        waitTimep95: Option[Int],
        queueType: QueueType,
    )

    sealed trait DeviceProvider 
    case object IBMDevice extends DeviceProvider
    case object RigettiDevice extends DeviceProvider
    case object QuEraDevice extends DeviceProvider
    case object IonQDevice extends DeviceProvider
    case object IQMDevice extends DeviceProvider
    case object Quantinuum extends DeviceProvider
    case object Pasqal extends DeviceProvider

    sealed trait QueueType 
    case object PriorityQueue extends QueueType
    case object NormalQueue extends QueueType

    def stringToDeviceProvider(provider: String) : DeviceProvider = 
        provider.toUpperCase() match {
            case "IBM" => IBMDevice
            case "RIGETTI" => RigettiDevice
            case "QUERA" => QuEraDevice
            case "IONQ" => IonQDevice
            case "IQM" => IQMDevice
            case "QUANTINUUM" => Quantinuum
            case "PASQAL" => Pasqal
        }
    
    def deviceProviderToString(provider: DeviceProvider) : String =
        provider match {
            case IBMDevice => "IBM"
            case RigettiDevice => "RIGETTI"
            case QuEraDevice => "QUERA"
            case IonQDevice => "IONQ"
            case IQMDevice => "IQM"
            case Quantinuum => "QUANTINUUM"
            case Pasqal => "PASQAL"
        }

    def stringToQueueType(qtype: String) : QueueType = 
        qtype.toUpperCase() match {
            case "PRIORITY" => PriorityQueue
            case "NORMAL" => NormalQueue
        }

    def queueTypeToString(qtype: QueueType) : String =
        qtype match {
            case PriorityQueue => "PRIORITY"
            case NormalQueue => "NORMAL"
        }

}