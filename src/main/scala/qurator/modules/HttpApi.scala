package qurator.modules

import scala.concurrent.duration._

import cats.effect.Async
import cats.syntax.all._
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware._
import org.http4s.headers.Origin
import java.net.http.HttpClient
import qurator.http.DataRoutes
import qurator.http.version

object HttpApi {
  def make[F[_]: Async](
      services: Services[F],
  ): HttpApi[F] =
    new HttpApi[F](services) {}
}

sealed abstract class HttpApi[F[_]: Async] private (
    services: Services[F]
) {

    private val dpRoutes       = DataRoutes[F](services.dataPersistanceService).routes

    private val openRoutes: HttpRoutes[F] = dpRoutes

    private val routes: HttpRoutes[F] = Router(
        version.v1            -> openRoutes,
    )

    private val middleware: HttpRoutes[F] => HttpRoutes[F] = {
        { http: HttpRoutes[F] =>
        AutoSlash(http)
        } andThen { http: HttpRoutes[F] =>
        CORS.policy
            .withAllowOriginHost(Set(Origin.Host(Uri.Scheme.http, Uri.RegName("localhost"), Some(4200))))
            .withAllowCredentials(true)
            .apply(routes)
        } andThen { http: HttpRoutes[F] =>
        Timeout(60.seconds)(http)
        }
    }

    private val loggers: HttpApp[F] => HttpApp[F] = {
        { http: HttpApp[F] =>
        RequestLogger.httpApp(true, true)(http)
        } andThen { http: HttpApp[F] =>
        ResponseLogger.httpApp(true, true)(http)
        }
    }

    val httpApp: HttpApp[F] = loggers(middleware(routes).orNotFound)

}