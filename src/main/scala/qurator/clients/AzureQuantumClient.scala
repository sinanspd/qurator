package qurator.clients

import cats.syntax.all._
import eu.timepit.refined.auto._
import org.http4s.Method._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.http4s.client._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.util.CaseInsensitiveString
import cats.effect.kernel.MonadCancelThrow
import java.nio.charset.StandardCharsets
import fs2.Chunk
import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import java.time.ZoneOffset
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.security.MessageDigest
import org.http4s.ember.client.EmberClientBuilder
import cats.effect.IO
import qurator.domain.Braket.BraketDeviceDetailsResponse
import cats.effect.{Async, Concurrent}
import fs2.hashing.Hashing
import cats.effect.Sync
import org.typelevel.log4cats.Logger
import qurator.domain.ID
import qurator.domain.DeviceQueueInformation.DeviceQueueInformationId
import io.circe.syntax._ 
import qurator.domain.Azure._
import qurator.domain.calibration._


/**
 * This client is a WIP. Azure doesn't expose crucial information the scheduler needs, and therefore has been dropped from the initial implementation 
 **/ 
trait AzureQuantumClient[F[_]] {
   //def fetchBearerToken: F[String] // This is more secure but requires tremendous setup in Azure. Implement later.
   def fetchDeviceInformation: F[AzureDeviceStatusResponse]
   def submitJob(jobId: String, jobRequest: AzureJobCreateRequest): F[AzureJobResponse]
   def getQuantumTask(jobId: String): F[AzureJobResponse]
   def fetchDeviceCalibration(deviceId: String): F[DeviceCalibration]
}

object AzureQuantumClient {
  def make[F[_]: JsonDecoder: MonadCancelThrow : Logger](
      cfg: AzureConfig,
      client: Client[F]
  ): AzureQuantumClient[F] =
    new AzureQuantumClient[F] with Http4sClientDsl[F] { 

        def fetchDeviceInformation: F[AzureDeviceStatusResponse] = 
            Logger[F].info(s"Fetching Azure Quantum device information") *>
            Uri.fromString(s"https://eastus.quantum.azure.com/subscriptions/${cfg.subId}/resourceGroups/${cfg.resourceGroup}/providers/Microsoft.Quantum/workspaces/${cfg.workspace}/providerStatus?api-version=2025-09-01-preview")
            .liftTo[F].flatMap { uri =>
                val req = GET(
                    uri,
                    Header.Raw(CaseInsensitiveString("Accept"), "application/json"),
                    Header.Raw(CaseInsensitiveString("x-ms-quantum-api-key"), cfg.apiKey.value),
                )

                client.run(req).use { resp =>
                    resp.status match {
                        case Status.Ok =>
                        resp.asJsonDecode[AzureDeviceStatusResponse]
                        case st =>
                             MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed to fetch device details")
                            )
                    }
                }
            }

        def submitJob(jobId: String, jobRequest: AzureJobCreateRequest): F[AzureJobResponse] = 
            Logger[F].info(s"Submitting Azure Quantum job with ID: $jobId") *> 
                Uri.fromString(s"https://eastus.quantum.azure.com/subscriptions/${cfg.subId}/resourceGroups/${cfg.resourceGroup}/providers/Microsoft.Quantum/workspaces/${cfg.workspace}/jobs/${jobId}?api-version=2025-09-01-preview")
                .liftTo[F].flatMap{uri => 
                    val req = Request[F](
                                method = Method.PUT,
                                uri = uri,
                            ).withHeaders(
                                Headers(
                                Header.Raw(CaseInsensitiveString("Accept"), "application/json"),
                                Header.Raw(CaseInsensitiveString("x-ms-quantum-api-key"), cfg.apiKey.value),
                                )
                            )
                            .withEntity(jobRequest.asJson)

                    client.run(req).use { resp => 
                        resp.status match {
                            case Status.Accepted | Status.Ok =>
                                resp.asJsonDecode[AzureJobResponse]
                            case _ =>  MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed")
                            )
                        }
                    }
                }

        def getQuantumTask(jobId: String): F[AzureJobResponse] = 
            Logger[F].info(s"Fetching Azure Quantum job status for job ID: $jobId") *>
            Uri.fromString(s"https://eastus.quantum.azure.com/subscriptions/${cfg.subId}/resourceGroups/${cfg.resourceGroup}/providers/Microsoft.Quantum/workspaces/${cfg.workspace}/jobs/${jobId}?api-version=2025-09-01-preview")
            .liftTo[F].flatMap { uri =>
                val req = GET(
                    uri,
                    Header.Raw(CaseInsensitiveString("Accept"), "application/json"),
                    Header.Raw(CaseInsensitiveString("x-ms-quantum-api-key"), cfg.apiKey.value),
                )

                client.run(req).use { resp =>
                    resp.status match {
                        case Status.Ok =>
                            resp.asJsonDecode[AzureJobResponse]
                        case _ =>  MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed")
                            )
                    }
                }
            } 

        def fetchDeviceCalibration(deviceId: String): F[DeviceCalibration] = ??? 
    }   
}


        