package qurator.service

import java.util.UUID
import qurator.effects.GenUUID
import qurator.domain.DeviceQueueInformation._
import qurator.domain.SubmittedJobData._
import cats.effect._
import org.typelevel.log4cats.Logger
import skunk._
import skunk.implicits._
import skunk.data.Arr
import java.time.ZoneId
import qurator.domain.ID
import cats.syntax.all._
import qurator.sql.codecs._
import skunk.codec.text.varchar
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.Instant
import skunk.codec.all._
import eu.timepit.refined.auto._
import java.time.LocalDateTime

trait DataPersistanceService[F[_]] {
    def getDeviceQueueInformationByPage(page: Int): F[List[DeviceQueueInformation]]
    def persistDeviceQueueInformation(l : List[DeviceQueueInformationCreate]): F[Unit]
    def fetchQueueInformationAfterDate(date: LocalDateTime, device: String): F[List[DeviceQueueInformation]]
    def getQueueMinAfterDateForDevice(date: LocalDateTime, device: String): F[Option[DeviceQueueInformation]]
    def getQueueMaxAfterDateForDevice(date: LocalDateTime, device: String): F[Option[DeviceQueueInformation]]
    def persistSubmittedJobData(data: SubmittedJobDataCreate): F[Unit]
    def fetchSubmittedJobDataAfterDate(date: LocalDateTime, provider: String, deviceId: String): F[List[SubmittedJobData]]
}

object DataPersistanceService {
  def make[F[_]: GenUUID: Concurrent: Logger](
      postgres: Resource[F, Session[F]]
  ): DataPersistanceService[F] =
    new DataPersistanceService[F] {
      import DataPersistanceServiceSQL._

      private def ensureSubmittedJobDataTable: F[Unit] =
        postgres.use { session =>
          session.execute(createSubmittedJobDataTable).void
        }

      def persistDeviceQueueInformation(l : List[DeviceQueueInformationCreate]) = 
        for {
            _ <- Logger[F].info("Persisting device queue information")
            listofargs: F[List[
            (
                DeviceQueueInformationId ~
                String ~
                DeviceProvider ~
                Int ~
                Option[Int] ~
                Option[Int] ~
                Option[Int] ~
                QueueType ~
                LocalDateTime
            )
          ]] = l.map(
              i =>
                ID.make[F, DeviceQueueInformationId].map { id =>
                  val nw                   = LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault())
                  (id ~  i.name ~ i.provider ~ i.queueLength ~ i.waitTimeAvg ~ i.waitTimep50 ~ i.waitTimep95 ~ i.queueType ~ nw)
                }
            )
            .traverse(identity)
          args <- listofargs
          qry = {
            val enc =
            (deviceQueueInformationId ~ varchar ~ deviceProvider ~ int4 ~ int4.opt ~ int4.opt ~ int4.opt ~ queueType ~ timestamp).values
                .list(args)
            sql"""
                      INSERT INTO device_queue_info
                      VALUES $enc
                      """.command
          }
          a <- postgres.use { session =>
            session.prepare(qry).flatMap { cmd =>
              cmd.execute(args)
            }.void
          }
        } yield a 


      def getDeviceQueueInformationByPage(page: Int): F[List[DeviceQueueInformation]] = 
        Logger[F].info(s"Fetching device queue information page $page") *> 
        postgres.use { session =>
            session.prepare(fetchByPage).flatMap { cmd =>
              cmd.stream(page, 1024).compile.toList
            }
        }

      def fetchQueueInformationAfterDate(date: LocalDateTime, device: String): F[List[DeviceQueueInformation]] = 
        Logger[F].info(s"Fetching device queue information after $date") *> 
        postgres.use { session => 
          session.prepare(fetchAfterDate).flatMap { cmd =>
            cmd.stream(date ~ device, 1024).compile.toList
          }
        }

      def getQueueMinAfterDateForDevice(date: LocalDateTime, device: String): F[Option[DeviceQueueInformation]] = 
        Logger[F].info(s"Fetching min device queue information after $date") *> 
        postgres.use { session => 
          session.prepare(fetchMinAfterDateForDevice).flatMap { cmd =>
            cmd.option(date ~ device)
          }
        }

      def getQueueMaxAfterDateForDevice(date: LocalDateTime, device: String): F[Option[DeviceQueueInformation]] = 
        Logger[F].info(s"Fetching max device queue information after $date") *>
        postgres.use { session => 
          session.prepare(fetchMinAfterDateForDevice).flatMap { cmd =>
            cmd.option(date ~ device)
          }
        }

      def persistSubmittedJobData(data: SubmittedJobDataCreate): F[Unit] =
        for {
          _ <- ensureSubmittedJobDataTable
          _ <- Logger[F].info(s"Persisting submitted job data for provider=${data.provider}, device=${data.deviceId}, job=${data.jobId}")
          id <- ID.make[F, SubmittedJobDataId]
          args =
            id ~
            data.jobId ~
            data.provider ~
            data.deviceId ~
            data.submittedAt ~
            data.startedAt ~
            data.completedAt
          _ <- postgres.use { session =>
            session.prepare(insertSubmittedJobData).flatMap { cmd =>
              cmd.execute(args)
            }.void
          }
        } yield ()

      def fetchSubmittedJobDataAfterDate(
          date: LocalDateTime,
          provider: String,
          deviceId: String
      ): F[List[SubmittedJobData]] =
        ensureSubmittedJobDataTable *>
        Logger[F].info(s"Fetching submitted job data after $date for provider=$provider, device=$deviceId") *>
        postgres.use { session =>
          session.prepare(fetchSubmittedJobDataAfterDateQuery).flatMap { cmd =>
            cmd.stream(date ~ provider ~ deviceId, 1024).compile.toList
          }
        }
    }
}


private object DataPersistanceServiceSQL{
    val decoder : Decoder[DeviceQueueInformation] = 
        (deviceQueueInformationId ~ varchar ~ deviceProvider ~ int4 ~ int4.opt ~ int4.opt ~ int4.opt ~ queueType ~ timestamp)
        .map {
            case dqi ~ n ~ p ~ ql ~ wa ~ w5 ~ w9 ~ qt ~ ca =>
            DeviceQueueInformation(dqi, n, p, ql, wa, w5, w9, qt, ca)
        }

    val submittedJobDataDecoder: Decoder[SubmittedJobData] =
        (submittedJobDataId ~ varchar ~ varchar ~ varchar ~ timestamp ~ timestamp ~ timestamp)
        .map {
            case id ~ jobId ~ provider ~ deviceId ~ submittedAt ~ startedAt ~ completedAt =>
            SubmittedJobData(id, jobId, provider, deviceId, submittedAt, startedAt, completedAt)
        }

    val submittedJobDataEncoder: Encoder[
        SubmittedJobDataId ~ String ~ String ~ String ~ LocalDateTime ~ LocalDateTime ~ LocalDateTime
    ] =
        submittedJobDataId ~ varchar ~ varchar ~ varchar ~ timestamp ~ timestamp ~ timestamp

    val createSubmittedJobDataTable: Command[Void] =
        sql"""
         CREATE TABLE IF NOT EXISTS submitted_job_data (
           uuid uuid PRIMARY KEY,
           job_id varchar NOT NULL,
           provider varchar NOT NULL,
           device_id varchar NOT NULL,
           submitted_at timestamp NOT NULL,
           started_at timestamp NOT NULL,
           completed_at timestamp NOT NULL,
           UNIQUE (provider, job_id)
         )
        """.command

    val insertSubmittedJobData: Command[
        SubmittedJobDataId ~ String ~ String ~ String ~ LocalDateTime ~ LocalDateTime ~ LocalDateTime
    ] =
        sql"""
         INSERT INTO submitted_job_data (
           uuid,
           job_id,
           provider,
           device_id,
           submitted_at,
           started_at,
           completed_at
         )
         VALUES ($submittedJobDataEncoder)
         ON CONFLICT (provider, job_id) DO NOTHING
        """.command

    val fetchByPage: Query[Int, DeviceQueueInformation] =
        sql"""
         SELECT uuid, name, provider, queue_length, wait_time_avg, wait_time_p50, wait_time_p95, queue_type, created_at
         FROM device_queue_info
         ORDER BY created_at DESC
         OFFSET $int4 LIMIT 1000
        """.query(decoder)

    val fetchAfterDate: Query[LocalDateTime ~ String, DeviceQueueInformation] =
        sql"""
         SELECT uuid, name, provider, queue_length, wait_time_avg, wait_time_p50, wait_time_p95, queue_type, created_at
         FROM device_queue_info
         WHERE created_at > $timestamp AND name = $varchar
         ORDER BY created_at DESC
        """.query(decoder)

    val fetchMinAfterDateForDevice: Query[LocalDateTime ~ String, DeviceQueueInformation] =
        sql"""
         SELECT uuid, name, provider, queue_length, wait_time_avg, wait_time_p50, wait_time_p95, queue_type, created_at
         FROM device_queue_info
         WHERE created_at > $timestamp AND name = $varchar
         ORDER BY queue_length ASC
         LIMIT 1
        """.query(decoder)

    val fetchMaxAfterDateForDevice: Query[LocalDateTime ~ String, DeviceQueueInformation] =
        sql"""
         SELECT uuid, name, provider, queue_length, wait_time_avg, wait_time_p50, wait_time_p95, queue_type, created_at
         FROM device_queue_info
         WHERE created_at > $timestamp AND name = $varchar
         ORDER BY queue_length DESC
         LIMIT 1
        """.query(decoder)

    val fetchSubmittedJobDataAfterDateQuery: Query[LocalDateTime ~ String ~ String, SubmittedJobData] =
        sql"""
         SELECT uuid, job_id, provider, device_id, submitted_at, started_at, completed_at
         FROM submitted_job_data
         WHERE submitted_at > $timestamp AND provider = $varchar AND device_id = $varchar
         ORDER BY submitted_at DESC
         LIMIT 100
        """.query(submittedJobDataDecoder)
}
