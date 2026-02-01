package qurator.testbed

import cats.effect._
import cats.syntax.all._
import cats.effect.kernel.Ref
import qurator.domain.Task._
import java.time.LocalDateTime
import qurator.programs.DeviceEstimator
import scala.util.Random

final case class FakeDevice[F[_]: Async](name: String, deviceEstimator: DeviceEstimator[F]) {

  val jobs = Ref[F].of(List.empty[(QuantumTask, Int, Int, LocalDateTime)])

  def submitJob(t: QuantumTask, device: String): F[Unit] = for{
    j <- jobs
    currentJobs <- j.get
    now = LocalDateTime.now()
    queueLength <- deviceEstimator.estimateDeviceQueueLength(device)
    (queueSpeedMin, queueSpeedMax) <- deviceEstimator.estimateDeviceProcessingSpeed(device)
    updatedJobs = currentJobs :+ ((t, queueLength, Random.between(queueSpeedMin, queueSpeedMax), now))
  } yield ()

  def checkJobStatus(taskId: TaskId): F[Option[Boolean]] = for{
     j <- jobs
     currentJobs <- j.get
     jobOpt = currentJobs.find(_._1.uuid == taskId)
     job = jobOpt.get
     currentTime = LocalDateTime.now()
     totalTime = currentJobs.foldLeft((0, false))((acc, job) => {
        if(job._1.uuid == taskId){
            (acc._1, true)
        } else {
            (acc._1 + job._2, acc._2)
        }
     })
     status = (job._4.plusSeconds(totalTime._1)).isBefore(currentTime)
  } yield Some(status)
}