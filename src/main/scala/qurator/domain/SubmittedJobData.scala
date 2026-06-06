package qurator.domain

import qurator.optics.uuid
import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import io.estatico.newtype.macros.newtype

import java.time.{Duration, LocalDateTime}
import java.util.UUID

object SubmittedJobData {

    @derive(decoder, encoder, eqv, show, uuid)
    @newtype
    case class SubmittedJobDataId(
        value: UUID
    )

    @derive(decoder, encoder, eqv)
    case class SubmittedJobData(
        uuid: SubmittedJobDataId,
        jobId: String,
        provider: String,
        deviceId: String,
        submittedAt: LocalDateTime,
        startedAt: LocalDateTime,
        completedAt: LocalDateTime
    ) {
        def queueWaitMillis: Option[Long] = {
            val millis = Duration.between(submittedAt, startedAt).toMillis
            Option.when(millis >= 0L)(millis)
        }
    }

    @derive(decoder, encoder, eqv)
    case class SubmittedJobDataCreate(
        jobId: String,
        provider: String,
        deviceId: String,
        submittedAt: LocalDateTime,
        startedAt: LocalDateTime,
        completedAt: LocalDateTime
    )
}
