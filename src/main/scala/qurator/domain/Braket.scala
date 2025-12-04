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

object Braket{

    case class BraketConfig(
        accessId: NonEmptyString,
        apiSecret: Secret[NonEmptyString]
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketDeviceListResponse(
        devices: List[BraketDevice],
        nextToken: Option[String]
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketDevice(
        deviceArn: String,
        deviceName: String,
        deviceStatus: String,
        deviceType: String,
        providerName: String
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketDeviceDetailsResponse(
        deviceArn: String,
        deviceName: String,
        deviceStatus: String,
        deviceType: String,
        providerName: String,
        deviceCapabilities: String,
        deviceQueueInfo: List[BraketDeviceQueueInfo]
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketDeviceQueueInfo(
        queue: String,
        queuePriority: Option[String],
        queueSize: String
    )


    def toDeviceQueueInformation(l: List[BraketDeviceDetailsResponse]) = 
        l.filter(d => d.deviceStatus != "OFFLINE" && d.deviceType != "SIMULATOR" && d.deviceStatus != "RETIRED").flatMap(d => 
            d.deviceQueueInfo.map(q => 
                DeviceQueueInformationCreate(
                name = d.deviceArn,
                provider = stringToDeviceProvider(d.providerName),
                queueLength = d.deviceQueueInfo.headOption.map(q => q.queueSize.toInt).getOrElse(0),
                waitTimeAvg = None,
                waitTimep50 = None,
                waitTimep95 = None,
                queueType = q.queuePriority.map(p => stringToQueueType(p)).getOrElse(NormalQueue)
            ))
        )

    @derive(decoder, encoder, eqv, show)
    case class BraketCreateQuantumTaskRequest(
        action: String,
        associations: Option[List[BraketAssociation]],
        clientToken: String,
        deviceArn: String,
        deviceParameters: String,
        jobToken: Option[String] = None,
        outputS3Bucket: Option[String] = None,
        outputS3KeyPrefix: Option[String] = None,
        shots: Int,        
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketAssociation(
        arn: String,
        `type`: String
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketCreateQuantumTaskResponse(
    quantumTaskArn: String
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketOpenQasmProgram(
        braketSchemaHeader: BraketOpenQasmHeader,
        source: String
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketOpenQasmHeader(
        name: String = "braket.ir.openqasm.program",
        version: String = "1"
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketQuantumTaskResponse(
        actionMetadata: BraketActionMetadata,
        associations: List[BraketAssociation],
        createdAt: String,
        deviceArn: String,
        deviceParameters: String,
        endedAt: Option[String],
        experimentalCapabilities: Option[Map[String, String]],
        failureReason: Option[String],
        //jobArn: String,
        numSuccessfulShots: Int,
        outputS3Bucket: Option[String],
        outputS3Directory: Option[String],
        quantumTaskArn: String,
        queueInfo: BraketDeviceQueueInfo,
        shots: Int,
        status: String,
        tags: Option[Map[String, String]]
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketActionMetadata(
        actionType: String,
        executableCount: Int,
        programCount: Int
    )


//    "experimentalCapabilities": { ... },
//    "tags": { 
//       "string" : "string" 
//    }
}