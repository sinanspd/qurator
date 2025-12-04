package qurator.service

import java.util.UUID
import qurator.effects.GenUUID
import qurator.domain.DeviceQueueInformation._
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

trait DataPersistanceService[F[_]] {
    def getDeviceQueueInformationByPage(page: Int): F[List[DeviceQueueInformation]]
    def persistDeviceQueueInformation(l : List[DeviceQueueInformationCreate]): F[Unit]
}

object DataPersistanceService {
  def make[F[_]: GenUUID: Concurrent: Logger](
      postgres: Resource[F, Session[F]]
  ): DataPersistanceService[F] =
    new DataPersistanceService[F] {
      import DataPersistanceServiceSQL._

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
    }
}


private object DataPersistanceServiceSQL{
    val decoder : Decoder[DeviceQueueInformation] = 
        (deviceQueueInformationId ~ varchar ~ deviceProvider ~ int4 ~ int4.opt ~ int4.opt ~ int4.opt ~ queueType ~ timestamp)
        .map {
            case dqi ~ n ~ p ~ ql ~ wa ~ w5 ~ w9 ~ qt ~ ca =>
            DeviceQueueInformation(dqi, n, p, ql, wa, w5, w9, qt, ca)
        }

    val fetchByPage: Query[Int, DeviceQueueInformation] =
        sql"""
         SELECT uuid, name, provider, queue_length, wait_time_avg, wait_time_p50, wait_time_p95, queue_type, created_at
         FROM device_queue_info
         ORDER BY created_at DESC
         OFFSET $int4 LIMIT 1000
        """.query(decoder)
}