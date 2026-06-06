package qurator.domain

import qurator.domain.Task.QuantumTask
import qurator.domain.calibration.DeviceCalibration
import qurator.domain.circuit.Circuit
import qurator.domain.device.Device
import cats.data.NonEmptyList
import cats.{Functor, Monad}
import cats.syntax.all._
import java.time.LocalDateTime

trait ProviderTaskSubmission {
  def jobId: String
}

trait ProviderTaskStatus {
  def taskStatus: String
}

final case class ProviderJobTiming(
    startedAt: Option[LocalDateTime],
    completedAt: Option[LocalDateTime]
)

final case class ProviderBatchTask(
    task: QuantumTask,
    compiled: Circuit
)

final case class ProviderBatchSubmission(
    batchId: String,
    submissions: List[ProviderTaskSubmission]
) {
  def jobIds: List[String] =
    submissions.map(_.jobId)
}

trait ProviderBatchSubmitter[F[_]] {
  def submitBatch(
      device: Device,
      tasks: NonEmptyList[ProviderBatchTask]
  ): F[ProviderBatchSubmission]
  def closeBatch(batchId: String): F[Unit]
}

trait ProviderDeviceSummary {
  def platformId: String
  def isAvailable: Boolean
}

trait ProviderDeviceDetails {
  def platformId: String
  def toDevice: Device
}

trait ProviderDeviceList[+A <: ProviderDeviceSummary] {
  def devices: List[A]

  final def availableDeviceIds: List[String] =
    devices.collect { case device if device.isAvailable => device.platformId }
}

trait ProviderClient[F[_]] {
  def provider: String
  def batchSubmitter: Option[ProviderBatchSubmitter[F]] =
    None
  final def supportsBatchSubmissions: Boolean =
    batchSubmitter.isDefined
  def fetchAvailableDevices: F[List[Device]]
  def submitTask(
      device: Device,
      task: QuantumTask,
      compiled: Circuit
  ): F[_ <: ProviderTaskSubmission]
  def getTask(taskId: String): F[_ <: ProviderTaskStatus]
  def fetchJobTiming(taskId: String, status: ProviderTaskStatus): F[ProviderJobTiming]
  def fetchTaskResult(taskId: String, status: ProviderTaskStatus): F[QuantumJobResult]
  def fetchDeviceCalibration(deviceId: String): F[DeviceCalibration]
  def completedStatuses: Set[String]
}

object ProviderClient {
  def submitQuantumTask[F[_]: Functor](
      client: ProviderClient[F],
      device: Device,
      task: QuantumTask,
      compiled: Circuit
  ): F[String] =
    client.submitTask(device, task, compiled).map(_.jobId)

  def submitQuantumTaskBatch[F[_]](
      client: ProviderClient[F],
      device: Device,
      tasks: NonEmptyList[ProviderBatchTask]
  ): Option[F[ProviderBatchSubmission]] =
    client.batchSubmitter.map(_.submitBatch(device, tasks))

  def getTaskStatus[F[_]: Functor](client: ProviderClient[F], taskId: String): F[String] =
    client.getTask(taskId).map(_.taskStatus)

  def fetchQuantumTaskResult[F[_]: Monad](client: ProviderClient[F], taskId: String): F[QuantumJobResult] =
    client.getTask(taskId).flatMap(status => client.fetchTaskResult(taskId, status))

  def isTaskComplete[F[_]: Functor](client: ProviderClient[F], taskId: String): F[Boolean] =
    getTaskStatus(client, taskId).map(client.completedStatuses.contains)
}
