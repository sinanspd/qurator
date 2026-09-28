package qurator.domain

import qurator.optics.{IsUUID, uuid}
import io.circe.generic.auto.*
import cats.derived.*
import cats.Show
import cats.Eq
import java.time.{Duration, LocalDateTime}
import java.util.UUID

object SubmittedJobData {

    opaque type SubmittedJobDataId = UUID

    object SubmittedJobDataId {
        def apply(value: UUID): SubmittedJobDataId = value

        extension (id: SubmittedJobDataId) {
            def value: UUID = id
        }

        given Eq[SubmittedJobDataId] = Eq.fromUniversalEquals
        given Show[SubmittedJobDataId] = Show.fromToString
        given IsUUID[SubmittedJobDataId] = IsUUID.opaqueUUID[SubmittedJobDataId]
    }
  
    

    case class SubmittedJobData(
        uuid: SubmittedJobDataId,
        jobId: String,
        provider: String,
        deviceId: String,
        submittedAt: LocalDateTime,
        startedAt: LocalDateTime,
        completedAt: LocalDateTime
    ) derives Eq {
        def queueWaitMillis: Option[Long] = {
            val millis = Duration.between(submittedAt, startedAt).toMillis
            Option.when(millis >= 0L)(millis)
        }
    }

    case class SubmittedJobDataCreate(
        jobId: String,
        provider: String,
        deviceId: String,
        submittedAt: LocalDateTime,
        startedAt: LocalDateTime,
        completedAt: LocalDateTime
    ) derives Eq
}
