package qurator.modules

import qurator.Types._
import qurator.effects.GenUUID
import qurator.service._

import cats.effect._
import dev.profunktor.redis4cats.RedisCommands
import skunk.Session
import org.typelevel.log4cats.Logger

object Services {
  def make[F[_]: GenUUID: Temporal: Logger](
      postgres: Resource[F, Session[F]]
  ): Services[F] = {
    new Services[F](
     dataPersistanceService = DataPersistanceService.make[F](postgres)
    ) {}
  }
}

sealed abstract class Services[F[_]] private (
    val dataPersistanceService : DataPersistanceService[F]
)
