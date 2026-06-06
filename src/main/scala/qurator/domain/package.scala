package qurator

import java.time.LocalDateTime
import qurator.domain.DeviceQueueInformation.DeviceProvider
import qurator.domain.DeviceQueueInformation.QueueType
import cats.{ Eq, Monoid, Show }
import io.circe.{ Decoder, DecodingFailure, Encoder }
import io.circe.generic.extras.semiauto.{ deriveEnumerationDecoder, deriveEnumerationEncoder }
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import io.circe.parser
import io.circe.Error
import qurator.domain.Braket._
import io.circe.parser.decode

package object domain {
  type QuantumResult = QuantumJobResult
  
  implicit val deviceProviderDecoder: Decoder[DeviceProvider] =
    deriveEnumerationDecoder[DeviceProvider]

  implicit val deviceProviderEncoder: Encoder[DeviceProvider] =
    deriveEnumerationEncoder[DeviceProvider]

  implicit val queueTypeDecoder: Decoder[QueueType] =
    deriveEnumerationDecoder[QueueType]

  implicit val queueTypeEncoder: Encoder[QueueType] =
    deriveEnumerationEncoder[QueueType]

  implicit val dataTimeEq: Eq[LocalDateTime] = Eq.fromUniversalEquals

  def decodeExecutionWindows(deviceCapabilitiesJson: String): Either[Error, List[BraketExecutionWindows]] =
    decode[DeviceCapabilities](deviceCapabilitiesJson).map(_.service.executionWindows)

  def decodeQubitCount(deviceCapabilitiesJson: String): Either[Error, Int] =
    decode[DeviceCapabilities](deviceCapabilitiesJson).map { caps =>
      caps.paradigm.flatMap(p => p.qubitCount.orElse(p.modes)).getOrElse(0)
    }

  def deviceExecutionWindows(d: BraketDevice): Either[Error, List[BraketExecutionWindows]] =
    decodeExecutionWindows(d.deviceCapabilities)
}
