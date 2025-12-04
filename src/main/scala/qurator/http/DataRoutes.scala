package qurator.http

import cats.Monad
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import qurator.domain._
import qurator.service.DataPersistanceService
import eu.timepit.refined.auto._

import cats.MonadThrow
import cats.syntax.all._
import io.circe.JsonObject
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server._
import java.util.UUID

final case class DataRoutes[F[_]: JsonDecoder: MonadThrow](
    dp: DataPersistanceService[F]
) extends Http4sDsl[F] {

  private[http] val prefixPath = "/queue"

  object QueueParamMatcher  extends QueryParamDecoderMatcher[Int]("page")

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root :? QueueParamMatcher(p) => 
        dp.getDeviceQueueInformationByPage(p).flatMap { list =>
            Ok(list)
        }.recoverWith {
          case e =>
            println(e)
            NoContent()
        }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}