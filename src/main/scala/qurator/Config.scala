package qurator

import cats.effect.Async
import qurator.Types.AppConfig
import qurator.Types.*
import cats.effect.Async
import cats.syntax.all.*
import ciris.*
import ciris.refined.*
import com.comcast.ip4s.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.types.all.*
import eu.timepit.refined.types.string.NonEmptyString
import qurator.domain.IBM.IBMConfig
import scala.concurrent.duration.*
import qurator.domain.Braket.BraketConfig
import qurator.domain.Azure.AzureConfig
import qurator.domain.CutQC.CutQCConfig


object Config{

    private def nonEmptyString(s: String): NonEmptyString =
        eu.timepit.refined.refineV[NonEmpty](s)
          .fold(error => throw new Exception(error), identity)

    def load[F[_] : Async] : F[AppConfig] = 
        (env("IBM_SERVICE_CRN").as[NonEmptyString].or(env("IBM_INSTANCE_ID").as[NonEmptyString]),
         env("IBM_API_KEY").as[NonEmptyString].secret,
         env("SC_POSTGRES_PASSWORD").as[NonEmptyString].secret,
         env("AWS_ACCESS_ID").as[NonEmptyString],
         env("AWS_API_SECRET").as[NonEmptyString].secret,
         env("BRAKET_REGIONS").as[String].map(parseBraketRegions).default(BraketConfig.defaultRegions),
         env("AZURE_RESOURCE_GROUP").as[NonEmptyString],
         env("AZURE_SUB_ID").as[NonEmptyString],
         env("AZURE_WORKSPACE").as[NonEmptyString],
         env("AZURE_QUANTUM_API_KEY").as[NonEmptyString].secret,
         env("CUTQC_BASE_URI").as[NonEmptyString].default(nonEmptyString("http://localhost:8000")),
         env("QURATOR_DEVICE_FIT_QASM_FOLDER").as[NonEmptyString].option,
         env("QURATOR_DEVICE_FIT_OUTPUT").as[NonEmptyString].option,
         env("QURATOR_ENV").as[String].default("development")
        ).parMapN{(
            ibmInstanceId,
            ibmAPIkey,
            pgPassword,
            awsaccessid,
            awsapisecret,
            braketRegions,
            azureResource,
            azureSubId,
            azureWorkspace,
            azureApiKey,
            cutqcBaseUri,
            deviceFitQasmFolder,
            deviceFitOutput,
            environment
        ) => {
            AppConfig(
                PostgreSQLConfig(
                    host = nonEmptyString("qurator.cjy4iumyuob7.us-east-1.rds.amazonaws.com"), 
                    port = 5432.asInstanceOf[UserPortNumber],
                    user = nonEmptyString("postgres"),
                    password = pgPassword,
                    database = nonEmptyString("postgres"),
                    max = 10.asInstanceOf[PosInt]
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
                    apiSecret = awsapisecret,
                    regions = braketRegions
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
                deviceFitConfig = DeviceFitConfig(
                    qasmFolder = deviceFitQasmFolder,
                    output = deviceFitOutput
                ),
                environment = AppEnvironment.fromString(environment)
            )
        }}.load[F]

    private def parseCsvList(value: String): List[String] =
        value.split(",").iterator.map(_.trim).filter(_.nonEmpty).toList.distinct

    private def parseBraketRegions(value: String): List[String] = {
        val regions = parseCsvList(value)
        if (regions.nonEmpty) regions else BraketConfig.defaultRegions
    }
}
