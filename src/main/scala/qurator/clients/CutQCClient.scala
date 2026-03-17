package qurator.clients

import cats.effect.MonadCancelThrow
import cats.syntax.all._
import org.http4s._
import org.http4s.circe._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import qurator.domain.CutQC._

trait CutQCClient[F[_]] {
    def cut(r: CutRequest): F[CutResponse]
}

object CutQCClient {
    def make[F[_]: JsonDecoder: MonadCancelThrow](
        cfg: CutQCConfig,
        client: Client[F]
    ): CutQCClient[F] =
        new CutQCClient[F] with Http4sClientDsl[F] {
            def cut(r: CutRequest): F[CutResponse] = {
                val req = Request[F](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(s"${cfg.baseUri}/cut")
                ).withEntity(r)

                client.run(req).use { resp =>
                    resp.status match {
                        case Status.Ok =>
                            resp.asJsonDecode[CutResponse]
                        case _ =>
                            resp.asJsonDecode[ErrorResponse].flatMap { body =>
                                MonadCancelThrow[F].raiseError(
                                    new Exception(s"Circuit cutting failed: ${body.error.explain}")
                                )
                            }
                    }
                }
            }
        }
}
