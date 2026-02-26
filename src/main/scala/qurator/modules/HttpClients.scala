package qurator.modules


import cats.effect.MonadCancelThrow
import org.http4s.circe.JsonDecoder
import org.http4s.client.Client
import dev.profunktor.redis4cats.RedisCommands
import org.typelevel.log4cats.Logger
import cats.effect.Async
import qurator.clients._
import qurator.Types._
import fs2.hashing.Hashing

object HttpClients {
  def make[F[_]: JsonDecoder: MonadCancelThrow: Logger: Async : Hashing](
      cfg: AppConfig,
      client: Client[F]
  ): HttpClients[F] =
    new HttpClients[F] {
        def ibm: IBMClient[F] = IBMClient.make[F](cfg.ibmCredentials, client)
        def braket: BraketClient[F] = BraketClient.make[F](cfg.braketConfig, client)
        def azure: AzureQuantumClient[F] = AzureQuantumClient.make[F](cfg.azureConfig, client)  
    }
}

sealed trait HttpClients[F[_]] {
  def ibm: IBMClient[F]
  def braket: BraketClient[F]
  def azure: AzureQuantumClient[F]
}