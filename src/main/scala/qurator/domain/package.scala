package qurator

import java.time.LocalDateTime
import qurator.domain.DeviceQueueInformation.DeviceProvider
import qurator.domain.DeviceQueueInformation.QueueType
import cats.{ Eq, Monoid, Show }
import io.circe.{ Decoder, DecodingFailure, Encoder }
import io.circe.generic.extras.semiauto.{ deriveEnumerationDecoder, deriveEnumerationEncoder }
import io.circe.generic.semiauto._
import io.circe.generic.auto._

package object domain {
  
  implicit val deviceProviderDecoder: Decoder[DeviceProvider] =
    deriveEnumerationDecoder[DeviceProvider]

  implicit val deviceProviderEncoder: Encoder[DeviceProvider] =
    deriveEnumerationEncoder[DeviceProvider]

  implicit val queueTypeDecoder: Decoder[QueueType] =
    deriveEnumerationDecoder[QueueType]

  implicit val queueTypeEncoder: Encoder[QueueType] =
    deriveEnumerationEncoder[QueueType]

  implicit val dataTimeEq: Eq[LocalDateTime] = Eq.fromUniversalEquals
}
