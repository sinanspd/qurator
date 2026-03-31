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
import java.time.LocalDateTime
import scala.util.Try
import qurator.domain.device.Device

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
        deviceCapabilities: String,
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
    ){
        def toDevice: Device = {
            val q = decodeQubitCount(deviceCapabilities).getOrElse(0)
            Device(
                platform = "Braket",
                platformId = deviceArn, 
                q,
                deviceQueueInfo.headOption.flatMap(q => q.queueSize.toIntOption).getOrElse(0),
                t1 = 0.0f,
                t2 =  0.0f,
                gateSet = List()
            )
        }
    }

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

    @derive(decoder, encoder, eqv, show)
    case class DeviceCapabilities(
        service: DeviceCapabilitiesService, 
        paradigm: Option[BraketParadigm] = None 
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketParadigm(
        qubitCount: Option[Int] = None,
        modes: Option[Int] = None 
    )


    @derive(decoder, encoder, eqv, show)
    case class DeviceCapabilitiesService(
        braketSchemaHeader: BraketSchemaHeader,
        executionWindows: List[BraketExecutionWindows]
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketSchemaHeader(
        name: String,
        version: String
    )

    @derive(decoder, encoder, eqv, show)
    case class BraketExecutionWindows(
        executionDay: String,
        windowStartHour: String,
        windowEndHour: String
    )


    sealed trait BraketTimeParseError extends Product with Serializable
    object BraketTimeParseError {
        case object Empty extends BraketTimeParseError
        final case class BadFormat(input: String) extends BraketTimeParseError
        final case class NotAnInt(part: String) extends BraketTimeParseError
        final case class OutOfRange(field: String, value: Int) extends BraketTimeParseError
    }
    def parseBraketHourMinute(raw: String): Either[BraketTimeParseError, (Int, Int)] = {
        val s = Option(raw).map(_.trim).getOrElse("")
        if (s.isEmpty) return Left(BraketTimeParseError.Empty)

        val parts = s.split(":", -1).toList
        parts match {
            case h :: m :: Nil =>
                for {
                    hh <- parseInt(h)
                    mm <- parseInt(m)
                    _  <- validateRange("hour", hh, 0, 23)
                    _  <- validateRange("minute", mm, 0, 59)
                } yield (hh, mm)

            case h :: m :: sec :: Nil =>
                for {
                    hh <- parseInt(h)
                    mm <- parseInt(m)
                    ss <- parseInt(sec)
                    _  <- validateRange("hour", hh, 0, 23)
                    _  <- validateRange("minute", mm, 0, 59)
                    _  <- validateRange("second", ss, 0, 59) // validated but ignored
                } yield (hh, mm)

            case _ =>
                Left(BraketTimeParseError.BadFormat(s))
        }
    }

    private def parseInt(part: String): Either[BraketTimeParseError, Int] =
        Try(part.toInt).toEither.left.map(_ => BraketTimeParseError.NotAnInt(part))

    private def validateRange(field: String, value: Int, lo: Int, hi: Int): Either[BraketTimeParseError, Unit] =
        if (value >= lo && value <= hi) Right(())
        else Left(BraketTimeParseError.OutOfRange(field, value))

    def deviceActive(d: BraketDevice) = {
        val parseDeviceCapabilities = deviceExecutionWindows(d)
        parseDeviceCapabilities match {
            case Left(e) => false
            case Right(w) => 
                w.foldLeft(false)((a, b) => {
                    val today = LocalDateTime.now()
                    val day = today.getDayOfWeek().toString
                    val weekday = day != "SATURDAY" && day != "SUNDAY"
                    val hour = today.getHour()
                    val minute = today.getMinute()
                    val dayMatches = b.executionDay == "Everyday" || (b.executionDay == "Weekdays" && weekday) ||
                       (b.executionDay == "Weekends" && !weekday)  || b.executionDay.toUpperCase() == day
                    val parsedStart = parseBraketHourMinute(b.windowStartHour)
                    val parsedEnd =  parseBraketHourMinute(b.windowEndHour)
                    val timeMatches = (parsedStart, parsedEnd) match {
                        case (Right(s), Right(e)) => (s._1 <= hour && hour <= e._1) && (s._2 <= minute && minute < e._2)
                        case _ => false
                    }
                    timeMatches && dayMatches
                })
        }
    }
}