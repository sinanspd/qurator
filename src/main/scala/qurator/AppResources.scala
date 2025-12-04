package qurator

import cats.effect.std.Console
import cats.effect.{ Concurrent, Resource, Temporal }
import cats.syntax.all._
import dev.profunktor.redis4cats.effect.MkRedis
import dev.profunktor.redis4cats.{ Redis, RedisCommands }
import eu.timepit.refined.auto._
import fs2.io.net.Network
import natchez.Trace.Implicits.noop
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import skunk._
import skunk.codec.text._
import skunk.implicits._
import qurator.Types.AppConfig
import qurator.Types.PostgreSQLConfig

sealed abstract class AppResources[F[_]](
    val client: Client[F],
    val postgres: Resource[F, Session[F]]
)

object AppResources {

  def make[F[_]: Temporal: Console: Logger: MkHttpClient: Network](
      cfg: AppConfig
  ): Resource[F, AppResources[F]] = {

    def checkPostgresConnection(
        postgres: Resource[F, Session[F]]
    ): F[Unit] =
      postgres.use { session =>
        session.unique(sql"select version();".query(text)).flatMap { v =>
          Logger[F].info(s"Connected to Postgres $v")
        }
      }

    def mkPostgreSqlResource(c: PostgreSQLConfig): SessionPool[F] =
      Session
        .pooled[F](
          host = c.host.value,
          port = c.port.value,
          user = c.user.value,
          password = Some(c.password.value),
          database = c.database.value,
          max = c.max.value,
          ssl      = SSL.Trusted
        )
        .evalTap(checkPostgresConnection _)

    (
      MkHttpClient[F].newEmber(cfg.httpClientConfig),
      mkPostgreSqlResource(cfg.postgreSQL),
    ).parMapN(new AppResources[F](_, _) {})

  }

}
