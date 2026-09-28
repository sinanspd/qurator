package qurator.http

import cats.{Monad, MonadThrow}
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import io.circe.generic.auto.deriveEncoder
import io.circe.syntax.*
import qurator.domain.*
import qurator.service.DataPersistanceService
import eu.timepit.refined.auto.*
import java.util.UUID

final case class DataRoutes[F[_]: JsonDecoder: MonadThrow](
    dp: DataPersistanceService[F]
) extends Http4sDsl[F] {

  private[http] val prefixPath = "/queue"

  object QueueParamMatcher extends QueryParamDecoderMatcher[Int]("page")

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