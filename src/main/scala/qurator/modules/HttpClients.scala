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
import qurator.domain.ProviderClient

object HttpClients {
  def make[F[_]: JsonDecoder: MonadCancelThrow: Logger: Async : Hashing](
      cfg: AppConfig,
      client: Client[F]
  ): HttpClients[F] =
    new HttpClients[F] {
        private lazy val ibmClient = IBMClient.make[F](cfg.ibmCredentials, client)
        private lazy val braketClient = BraketClient.make[F](cfg.braketConfig, client)
        private lazy val azureClient = AzureQuantumClient.make[F](cfg.azureConfig, client)

        def ibm: IBMClient[F] = ibmClient
        def braket: BraketClient[F] = braketClient
        def azure: AzureQuantumClient[F] = azureClient
        def providerClients: List[ProviderClient[F]] = List(ibmClient, braketClient)
    }

  //only to be used by tests, we will eventually switch to mock routes and remove this. Don't use elsewhere. 
  private[qurator] def fromParts[F[_]](
      ibm0: IBMClient[F],
      braket0: BraketClient[F],
      azure0: AzureQuantumClient[F]
  ): HttpClients[F] =
    new HttpClients[F] {
      def ibm: IBMClient[F] = ibm0
      def braket: BraketClient[F] = braket0
      def azure: AzureQuantumClient[F] = azure0
      def providerClients: List[ProviderClient[F]] = List(ibm0, braket0)
    }
}

sealed trait HttpClients[F[_]] {
  def ibm: IBMClient[F]
  def braket: BraketClient[F]
  def azure: AzureQuantumClient[F]
  def providerClients: List[ProviderClient[F]]

  final def providerClient(platform: String): Option[ProviderClient[F]] =
    providerClients.find(_.provider == platform)
}
