package qurator.domain

import qurator.domain.Task.QuantumTask
import qurator.domain.calibration.DeviceCalibration
import qurator.domain.circuit.Circuit
import qurator.domain.device.Device
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

  def getTaskStatus[F[_]: Functor](client: ProviderClient[F], taskId: String): F[String] =
    client.getTask(taskId).map(_.taskStatus)

  def fetchQuantumTaskResult[F[_]: Monad](client: ProviderClient[F], taskId: String): F[QuantumJobResult] =
    client.getTask(taskId).flatMap(status => client.fetchTaskResult(taskId, status))

  def isTaskComplete[F[_]: Functor](client: ProviderClient[F], taskId: String): F[Boolean] =
    getTaskStatus(client, taskId).map(client.completedStatuses.contains)
}
