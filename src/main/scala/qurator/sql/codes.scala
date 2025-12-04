package qurator.sql


import java.util.UUID

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._

import skunk._
import skunk.codec.all._
import skunk.data.Type._uuid
import skunk.data.Arr
import squants.market._
import eu.timepit.refined.api.{ RefType, Validate }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats._
import eu.timepit.refined.string._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.boolean._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.numeric._
import eu.timepit.refined.api.{ RefType, Refined }
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.generic._
//import shop.ext.refined._
import scala.tools.nsc.doc.base.comment.Code
import qurator.domain.DeviceQueueInformation._


object codecs{

    val deviceQueueInformationId: Codec[DeviceQueueInformationId]           = uuid.imap[DeviceQueueInformationId](DeviceQueueInformationId(_))(_.value)
    val deviceProvider: Codec[DeviceProvider] =
        varchar.imap[DeviceProvider](stringToDeviceProvider(_))(deviceProviderToString(_))

    val queueType: Codec[QueueType] = 
        varchar.imap[QueueType](stringToQueueType(_))(queueTypeToString(_))
}