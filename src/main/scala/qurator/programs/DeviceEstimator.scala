package qurator.programs

import cats.MonadThrow
import cats.effect._
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import qurator.service.DataPersistanceService
import scala.util.Random

final case class DeviceEstimator[F[_]: Logger: MonadCancelThrow](
    persistanceService: DataPersistanceService[F]
){

    val devices = List()

    def estimateDeviceQueueLength(device: String): F[Int] = 
        for {
            _ <- Logger[F].info(s"Estimating device performance for device: $device")
            now = java.time.LocalDateTime.now()
            days10ago = now.minusDays(10)
            minQueueInfoOpt <- persistanceService.getQueueMinAfterDateForDevice(days10ago, device)
            maxQueueInfoOpt <- persistanceService.getQueueMaxAfterDateForDevice(days10ago, device)
            _ <- (minQueueInfoOpt, maxQueueInfoOpt) match {
                case (Some(minInfo), Some(maxInfo)) => 
                    Logger[F].info(s"Device: $device, Min Queue Length: ${minInfo.queueLength}, Max Queue Length: ${maxInfo.queueLength}")
                case _ => 
                    MonadCancelThrow[F].raiseError(
                        new Exception(s"Insufficient data to estimate performance for device: $device")
                    )
            }
        } yield Random.between(
            minQueueInfoOpt.map(_.queueLength).getOrElse(0), 
            maxQueueInfoOpt.map(_.queueLength).getOrElse(0) + 1
        )
    
    def estimateDeviceProcessingSpeed(device: String): F[(Int, Int)] = 
        for {
            _ <- Logger[F].info(s"Estimating device processing speed for device: $device")
            now = java.time.LocalDateTime.now()
            days10ago = now.minusDays(10)
            queueInfos <- persistanceService.fetchQueueInformationAfterDate(days10ago, device) 
            _ <- Logger[F].info(s"Fetched ${queueInfos.length} queue info records for device: $device")
            relevantInfos = queueInfos
                .filter(a => a.createdAt.getHour() > 9 && a.createdAt.getHour() < 19) 
            (bestValOpt, longestLen, _, _) =
                relevantInfos.map(_.queueLength).foldLeft((Option.empty[Int], 0, Option.empty[Int], 0)) {
                    case ((bestValOpt, bestLen, curValOpt, curLen), x) =>
                        if (x == 0) {
                            (bestValOpt, bestLen, None, 0)
                        } else {
                            curValOpt match {
                                case Some(v) if v == x =>
                                    val newLen = curLen + 1
                                    if (newLen > bestLen) (Some(x), newLen, Some(x), newLen)
                                    else                  (bestValOpt, bestLen, Some(x), newLen)
                                case _ =>
                                val newLen = 1
                                if (newLen > bestLen) (Some(x), newLen, Some(x), newLen)
                                else                  (bestValOpt, bestLen, Some(x), newLen)
                            }
                        }
                }
            largestGap = relevantInfos.map(_.queueLength).sliding(2).collect {
                case List(a, b) => a - b
            }.foldLeft(0)(Math.max)    
        } yield (longestLen, largestGap)
}