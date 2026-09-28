package qurator.domain

import io.circe.{Encoder, Decoder, Json}
import io.circe.generic.semiauto.{deriveEncoder, deriveDecoder}
import cats.derived.*
import cats.Show
import cats.Eq
import qurator.optics.IsUUID
import java.util.UUID
import java.time.LocalDateTime

object DeviceQueueInformation {

    opaque type DeviceQueueInformationId = UUID

    object DeviceQueueInformationId {
        def apply(value: UUID): DeviceQueueInformationId = value

        extension (id: DeviceQueueInformationId) {
            def value: UUID = id
        }

        given Eq[DeviceQueueInformationId] = Eq.fromUniversalEquals
        given Show[DeviceQueueInformationId] = Show.fromToString
        given IsUUID[DeviceQueueInformationId] = IsUUID.opaqueUUID[DeviceQueueInformationId]
        given Encoder[DeviceQueueInformationId] = Encoder.encodeUUID
        given Decoder[DeviceQueueInformationId] = Decoder.decodeUUID
    }

    case class DeviceQueueInformation(
       uuid: DeviceQueueInformationId,
       name: String,
       provider: DeviceProvider,
       queueLength: Int,
       waitTimeAvg: Option[Int],
       waitTimep50: Option[Int],
       waitTimep95: Option[Int],
       queueType: QueueType,
       createdAt: LocalDateTime
   ) derives Eq

    object DeviceQueueInformation {
        given Encoder[DeviceQueueInformation] = deriveEncoder
        given Decoder[DeviceQueueInformation] = deriveDecoder
    }

    case class DeviceQueueInformationCreate(
       name: String,
       provider: DeviceProvider,
       queueLength: Int,
       waitTimeAvg: Option[Int],
       waitTimep50: Option[Int],
       waitTimep95: Option[Int],
       queueType: QueueType,
    ) derives Eq, Show

    object DeviceQueueInformationCreate {
        given Encoder[DeviceQueueInformationCreate] = deriveEncoder
        given Decoder[DeviceQueueInformationCreate] = deriveDecoder
    }

    sealed trait DeviceProvider
    case object IBMDevice extends DeviceProvider
    case object RigettiDevice extends DeviceProvider
    case object QuEraDevice extends DeviceProvider
    case object IonQDevice extends DeviceProvider
    case object IQMDevice extends DeviceProvider
    case object Quantinuum extends DeviceProvider
    case object Pasqal extends DeviceProvider
    case object AQTDevice extends DeviceProvider

    object DeviceProvider {
        given Encoder[DeviceProvider] = Encoder.instance {
            case IBMDevice => Json.fromString("IBM")
            case RigettiDevice => Json.fromString("RIGETTI")
            case QuEraDevice => Json.fromString("QUERA")
            case IonQDevice => Json.fromString("IONQ")
            case IQMDevice => Json.fromString("IQM")
            case Quantinuum => Json.fromString("QUANTINUUM")
            case Pasqal => Json.fromString("PASQAL")
            case AQTDevice => Json.fromString("AQT")
        }

        given Decoder[DeviceProvider] = Decoder.decodeString.emap { str =>
            Right(stringToDeviceProvider(str))
        }
    }

    sealed trait QueueType
    case object PriorityQueue extends QueueType
    case object NormalQueue extends QueueType

    object QueueType {
        given Encoder[QueueType] = Encoder.instance {
            case PriorityQueue => Json.fromString("PRIORITY")
            case NormalQueue => Json.fromString("NORMAL")
        }

        given Decoder[QueueType] = Decoder.decodeString.emap { str =>
            Right(stringToQueueType(str))
        }
    }

    def stringToDeviceProvider(provider: String): DeviceProvider =
        provider.toUpperCase() match {
            case "IBM" => IBMDevice
            case "RIGETTI" => RigettiDevice
            case "QUERA" => QuEraDevice
            case "IONQ" => IonQDevice
            case "IQM" => IQMDevice
            case "QUANTINUUM" => Quantinuum
            case "PASQAL" => Pasqal
            case "AQT" => AQTDevice
            case _ => IBMDevice // default fallback
        }

    def deviceProviderToString(provider: DeviceProvider): String =
        provider match {
            case IBMDevice => "IBM"
            case RigettiDevice => "RIGETTI"
            case QuEraDevice => "QUERA"
            case IonQDevice => "IONQ"
            case IQMDevice => "IQM"
            case Quantinuum => "QUANTINUUM"
            case Pasqal => "PASQAL"
            case AQTDevice => "AQT"
        }

    def stringToQueueType(qtype: String): QueueType =
        qtype.toUpperCase() match {
            case "PRIORITY" => PriorityQueue
            case "NORMAL" => NormalQueue
            case _ => NormalQueue // default fallback
        }

    def queueTypeToString(qtype: QueueType): String =
        qtype match {
            case PriorityQueue => "PRIORITY"
            case NormalQueue => "NORMAL"
        }
}