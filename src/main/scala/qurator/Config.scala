package qurator

import cats.effect.Async
import qurator.Types.AppConfig
import qurator.Types._

import cats.effect.Async
import cats.syntax.all._
import ciris._
import ciris.refined._
import com.comcast.ip4s._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import qurator.domain.IBM.IBMConfig
import scala.concurrent.duration._
import qurator.domain.Braket.BraketConfig
import qurator.domain.Azure.AzureConfig
import qurator.domain.CutQC.CutQCConfig


object Config{

    def load[F[_] : Async] : F[AppConfig] = 
        (env("IBM_SERVICE_CRN").as[NonEmptyString].or(env("IBM_INSTANCE_ID").as[NonEmptyString]),
         env("IBM_API_KEY").as[NonEmptyString].secret,
         env("SC_POSTGRES_PASSWORD").as[NonEmptyString].secret,
         env("AWS_ACCESS_ID").as[NonEmptyString],
         env("AWS_API_SECRET").as[NonEmptyString].secret,
         env("AZURE_RESOURCE_GROUP").as[NonEmptyString],
         env("AZURE_SUB_ID").as[NonEmptyString],
         env("AZURE_WORKSPACE").as[NonEmptyString],
         env("AZURE_QUANTUM_API_KEY").as[NonEmptyString].secret,
         env("CUTQC_BASE_URI").as[NonEmptyString].default("http://localhost:8000"),
         env("QURATOR_ENV").as[String].default("development")
        ).parMapN{(ibmInstanceId, ibmAPIkey, pgPassword, awsaccessid, awsapisecret, azureResource, azureSubId, azureWorkspace, azureApiKey, cutqcBaseUri, environment) => {
            AppConfig(
                PostgreSQLConfig(
                    host = "qurator.cjy4iumyuob7.us-east-1.rds.amazonaws.com", 
                    port = 5432,
                    user = "postgres",
                    password = pgPassword,
                    database = "postgres",
                    max = 10
                ),
                IBMConfig(
                    instanceId = ibmInstanceId,
                    apiKey = ibmAPIkey
                ),
                 HttpClientConfig(
                    timeout = 60.seconds,
                    idleTimeInPool = 30.seconds
                ),
                BraketConfig(
                    accessId = awsaccessid,
                    apiSecret = awsapisecret
                ),
                HttpServerConfig(
                    host = host"0.0.0.0",
                    port = port"5000"//port"8080"
                ),
                AzureConfig(
                    subId = azureSubId,
                    resourceGroup = azureResource,
                    workspace = azureWorkspace,
                    apiKey = azureApiKey
                ),
                CutQCConfig(
                    baseUri = cutqcBaseUri
                ),
                environment = AppEnvironment.fromString(environment)
            )
        }}.load[F]
}
