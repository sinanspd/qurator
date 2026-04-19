package qurator.clients


import qurator.domain.Task._

sealed trait QuantumProviderClient[F[_]]{
    def submitTask(t: Task) : F[Unit]
    def getDeviceDetails(): F[Unit]
    def getTaskDetails(t: TaskId): F[Unit]
    def getDeviceCalibrationData(): F[Unit]
}