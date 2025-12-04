package qurator.clients


import cats.effect.MonadCancelThrow
import cats.syntax.all._
import eu.timepit.refined.auto._
import org.http4s.Method._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe._
import org.http4s.client._
import org.http4s.client.dsl.Http4sClientDsl
import qurator.domain.IBM.IBMConfig
import org.http4s.util.CaseInsensitiveString
import qurator.domain.IBM._
import org.typelevel.log4cats.Logger
import io.circe.generic.auto._
import cats.effect.Async


trait IBMClient[F[_]] {
  def fetchBearerToken: F[String]
  def fetchDeviceInformation: F[BackendsResponseV2]
  def submitJob(r: SubmitJobRequestV2): F[CreateJobResponseV2]
  def listJobDetails(id: String): F[JobDetailsResponseV2]
  def getJobMetrics(id: String): F[JobMetricsResponse]
}

object IBMClient {
  def make[F[_]: JsonDecoder: MonadCancelThrow : Logger : Async](
      cfg: IBMConfig,
      client: Client[F]
  ): IBMClient[F] =
    new IBMClient[F] with Http4sClientDsl[F] {
      def fetchBearerToken: F[String] = 
        Uri.fromString("https://iam.cloud.ibm.com/identity/token").liftTo[F].flatMap { uri =>
          val req = POST(
            UrlForm(
              "grant_type" -> "urn:ibm:params:oauth:grant-type:apikey",
              "apikey" -> cfg.apiKey.value
            ),
            uri,
            Header.Raw(CaseInsensitiveString("Content-Type"), "application/x-www-form-urlencoded")
          )

          client.run(req).use { resp =>
            resp.status match {
              case Status.Ok =>
                resp.asJsonDecode[IBMBearerToken].map(_.access_token)
            //   case st =>
            //     Error(
            //       Option(st.reason).getOrElse("unknown")
            //     ).raiseError[F, String]
            }
          }
        }

      def fetchDeviceInformation: F[BackendsResponseV2] = 
        fetchBearerToken.flatMap { token =>
          Uri.fromString(s"https://quantum.cloud.ibm.com/api/v1/backends").liftTo[F].flatMap { uri =>
            val req = GET(
              uri,
              Header.Raw(CaseInsensitiveString("Accept"), "application/json"),
              Header.Raw(CaseInsensitiveString("Authorization"), s"Bearer $token"),
              Header.Raw(CaseInsensitiveString("Service-CRN"), cfg.instanceId),
              Header.Raw(CaseInsensitiveString("IBM-API-Version"), "2025-05-01")
            )

            client.run(req).use { resp =>
              resp.status match {
                case Status.Ok =>
                  resp.asJsonDecode[BackendsResponseV2]
              //   case st =>
              //     Error(
              //       Option(st.reason).getOrElse("unknown")
              //     ).raiseError[F, BackendsResponseV2]
              }
            }
          }
        }

       def submitJob(r: SubmitJobRequestV2): F[CreateJobResponseV2] = 
          fetchBearerToken.flatMap { token =>
            val req = Request[F](
              method = Method.POST,
              uri = Uri.unsafeFromString("https://quantum.cloud.ibm.com/api/v1/jobs"),
            ).withHeaders(
                Headers(
                  Header.Raw(CaseInsensitiveString("Accept"), "application/json"),
                  Header.Raw(CaseInsensitiveString("Authorization"), s"Bearer $token"),
                  Header.Raw(CaseInsensitiveString("Service-CRN"), cfg.instanceId),
                  Header.Raw(CaseInsensitiveString("IBM-API-Version"), "2025-05-01")
                )
              )
              .withEntity(r) 

            client.run(req).use { resp =>
              resp.status match {
                case Status.Ok =>
                  resp.asJsonDecode[CreateJobResponseV2]
                case other =>
                    resp.as[String].flatMap { body =>
                        Logger[F].info(s"Status: $other, body: $body") *>
                            MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed to fetch device details for: $other")
                            )
                    }
              }
            }
        }

      def listJobDetails(id: String): F[JobDetailsResponseV2] = 
        fetchBearerToken.flatMap { token =>
          Uri.fromString(s"https://quantum.cloud.ibm.com/api/v1/jobs/$id").liftTo[F].flatMap { uri =>
            val req = GET(
              uri,
              Header.Raw(CaseInsensitiveString("Accept"), "application/json"),
              Header.Raw(CaseInsensitiveString("Authorization"), s"Bearer $token"),
              Header.Raw(CaseInsensitiveString("Service-CRN"), cfg.instanceId),
              Header.Raw(CaseInsensitiveString("IBM-API-Version"), "2025-05-01")
            )
            client.run(req).use { resp =>
              resp.status match {
                case Status.Ok =>
                  resp.asJsonDecode[JobDetailsResponseV2]
              }
            }
          }
        }

    def getJobMetrics(id: String): F[JobMetricsResponse] = 
      fetchBearerToken.flatMap { token =>
        Uri.fromString(s"https://quantum.cloud.ibm.com/api/v1/jobs/$id/metrics").liftTo[F].flatMap { uri =>
          val req = GET(
            uri,
            Header.Raw(CaseInsensitiveString("Accept"), "application/json"),
            Header.Raw(CaseInsensitiveString("Authorization"), s"Bearer $token"),
            Header.Raw(CaseInsensitiveString("Service-CRN"), cfg.instanceId),
            Header.Raw(CaseInsensitiveString("IBM-API-Version"), "2025-05-01")
          )
          client.run(req).use { resp =>
            resp.status match {
              case Status.Ok =>
                resp.asJsonDecode[JobMetricsResponse]
            }
          }
        }
      }
}}
