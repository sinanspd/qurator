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

object Azure{

    case class AzureConfig(
        subId: NonEmptyString,
        resourceGroup: NonEmptyString,
        workspace: NonEmptyString,
        apiKey: Secret[NonEmptyString]
    )  

    @derive(decoder, encoder, eqv, show)
    case class AzureDeviceStatusResponse(
        value: List[AzureDeviceStatus]
    )

    @derive(decoder, encoder, eqv, show)
    case class AzureDeviceStatus(
        currentAvailability: String, //ProviderAvailability, // Available, Degraded, Unavailable
        id: String,
        targets: List[AzureTargetStatus]
    ){
        def toDevice: Device = ???
    }

    @derive(decoder, encoder, eqv, show)
    case class AzureTargetStatus(
        averageQueueTime: Int,
        currentAvailability: String, //ProviderAvailability,
        id: String,
        statusPage: String
    )

    def toDeviceQueueInformation(l: List[AzureDeviceStatus]) = 
        l.flatMap(d => 
            d.targets.map(t => 
                DeviceQueueInformationCreate(
                    name = t.id,
                    provider = stringToDeviceProvider(d.id.toUpperCase()),  //this is terrible, find a better solution
                    queueLength = 0, //sadly they don't give us number of jobs 
                    waitTimeAvg = Some(t.averageQueueTime),
                    waitTimep50 = None,
                    waitTimep95 = None,
                    queueType = NormalQueue
                )
            )
        )

    // Available Target is available
    // Degraded  Target is available with degraded experience.
    // Unavailable	Target is unavailable.
    

    @derive(decoder, encoder, eqv, show)
    case class AzureJobCreateRequest(
        containerUri: String,
        itemType: String,
        name: String,
        providerId: String,
        target: String,
        inputDataFormat: Option[String] = None,
        inputDataUri: Option[String] = None,
        //inputParams: Option[Map[String, String]] = None, //TODO: Not supported yet
        jobType: Option[String] = None, //Unknown, QuantumComputing, Optimization 
        //metadata: Option[Map[String, String]] = None, //TODO: Not supported yet
        outputDataFormat: Option[String] = None,
        outputDataUri: Option[String] = None,
        priority: Option[Int] = None, //Standard, High
        sessionId: Option[String] = None,
        tags: Option[List[String]] = None
    )

    @derive(decoder, encoder, eqv, show)
    case class AzureJobResponse(
        beginExecutionTime: String,
        cancellationTime: Option[String],
        containerUri: String,
        costEstimate: Option[CostEstimate],
        creationTime: String,
        endExecutionTime: Option[String],
        //errorData: Option[WorkspaceItemError],
        id: String,
        inputDataFormat: Option[String], // It seems the most general we can go is "qir.v1". Some providers like quantinuum support "oneywell.openqasm.v1" but that isn't enough. I.e. doesn't seem like Pasqal does. It requires "pasqal.pulser.v1"
        inputDataUri: Option[String],
       //inputParams: Option[String],
        itemType: String,
        jobType: String, //Unknown, QuantumComputing, Optimization 
        //metadata: Option[String],
        name: String,
        outputDataFormat: Option[String],
        outputDataUri: Option[String],
        priority: String, //Standard, High
        providerId: String,
        quantumComputingData: Option[QuantumComputingData],
        sessionId: Option[String],
        status: String, // Executing, Succeeded, Failed, Canceled, Waiting
        tags: Option[List[String]],
        target: String,
        //usage: Option[String]
    )

    @derive(decoder, encoder, eqv, show)
    case class CostEstimate(
        currencyCode: String,
        estimatedTotal: Double,
        events: List[CostEstimateEvent]
    )

    @derive(decoder, encoder, eqv, show)
    case class CostEstimateEvent(
        description: Float,
        amountConsumed: Float,
        dimensionId: String,
        dimensionName: String,
        measureUnit: String,
        unitPrice: Float
    )

    @derive(decoder, encoder, eqv, show)
    case class QuantumComputingData(
        count: Int
    )

}