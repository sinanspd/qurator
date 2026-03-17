package qurator

import ciris._
import ciris.refined._
import com.comcast.ip4s.{ Host, Port }
import derevo.cats.show
import derevo.derive
import eu.timepit.refined.cats._
import eu.timepit.refined.types.net.UserPortNumber
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype
import qurator.domain.IBM._
import scala.concurrent.duration.FiniteDuration
import qurator.domain.Braket.BraketConfig
import qurator.domain.Azure.AzureConfig
import qurator.domain.CutQC.CutQCConfig

object Types{

    case class PostgreSQLConfig(
        host: NonEmptyString,
        port: UserPortNumber,
        user: NonEmptyString,
        password: Secret[NonEmptyString],
        database: NonEmptyString,
        max: PosInt
    )

    case class HttpClientConfig(
      timeout: FiniteDuration,
      idleTimeInPool: FiniteDuration
    )

    case class AppConfig(
        postgreSQL: Types.PostgreSQLConfig,
        ibmCredentials: IBMConfig,
        httpClientConfig: HttpClientConfig,
        braketConfig: BraketConfig,
        httpServerConfig: HttpServerConfig,
        azureConfig: AzureConfig,
        cutqcConfig: CutQCConfig
    )

    case class HttpServerConfig(
      host: Host,
      port: Port
    )
    
}
