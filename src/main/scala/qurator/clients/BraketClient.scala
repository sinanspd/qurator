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
import qurator.domain.IBM.IBMConfig
import org.http4s.util.CaseInsensitiveString
import qurator.domain.IBM._
import qurator.domain.Braket.BraketDeviceListResponse
import qurator.domain.Braket.BraketConfig
import cats.effect.kernel.MonadCancelThrow
import qurator.util.AWSSigner
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
import qurator.domain.Braket._
import cats.effect.Sync
import org.typelevel.log4cats.Logger
import qurator.domain.ID
import qurator.domain.DeviceQueueInformation.DeviceQueueInformationId
import io.circe.syntax._ 
import qurator.domain.calibration._

trait BraketClient[F[_]] {
  def fetchDeviceList: F[BraketDeviceListResponse]
  def fetchDeviceDetails(ids: List[String]): F[List[BraketDeviceDetailsResponse]]
  def submitBraketOpenQasmTask(r: BraketCreateQuantumTaskRequest, qasmSource:   String): F[BraketCreateQuantumTaskResponse] 
  // ^ TODO: This needs to be fixed so that the qasm source can be fetched from the request. Need to confirm the Braker IR first 
  def getQuantumTask(taskId: String) : F[BraketQuantumTaskResponse]
  def fetchDeviceCalibration(deviceArn: String): F[DeviceCalibration]
}
        

object BraketClient {
  def make[F[_]: Logger : JsonDecoder: MonadCancelThrow : Async : Concurrent : Hashing](
      cfg: BraketConfig,
      client: Client[F]
  ): BraketClient[F] =
    new BraketClient[F] with Http4sClientDsl[F] {

    def fetchDeviceList: F[BraketDeviceListResponse] = {
        val service   = "braket"
        val host      = s"braket.us-east-1.amazonaws.com"
        val uri       = Uri.unsafeFromString(s"https://$host/devices")
        val payloadS  = s"""{"filters": [],"maxResults": 100}"""
        val payload   = payloadS.getBytes(StandardCharsets.UTF_8)

        val signed = AWSSigner.signRequest(Method.POST, "us-east-1", service, host, "/devices", "", payload, cfg)
        
        val rawHeaders: List[Header.Raw] =
            signed.toList.map { case (k, v) => Header.Raw(CaseInsensitiveString(k), v) }

         val req = Request[F](
            method  = Method.POST,
            uri     = uri,
            headers = Headers(rawHeaders),
            body    = fs2.Stream.chunk(Chunk.array(payload))
        )

        client.run(req).use { resp =>
              resp.status match {
                case Status.Ok =>
                  resp.asJsonDecode[BraketDeviceListResponse]
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


    def  fetchDeviceDetails(ids: List[String]): F[List[BraketDeviceDetailsResponse]] = 
        ids.traverse { id =>
            val region  = "us-east-1"
            val service = "braket"
            val host    = s"$service.$region.amazonaws.com"
            val payload = Array.emptyByteArray
            val path    = AWSSigner.canonicalizePath(id)

            val headers = AWSSigner.signRequest(
                method  = Method.GET,
                region  = region,
                service = service,
                host    = host,
                basePath = "/device",
                rawPath = id,
                payload = payload,
                creds   = cfg
            )

            val req = Request[F](
                method  = Method.GET,
                uri     = Uri.unsafeFromString(s"https://$host/device/$path"),
                headers = Headers(headers.toList.map { case (k, v) => Header.Raw(CaseInsensitiveString(k), v) })
            )

            implicit val userDecoder: EntityDecoder[F, BraketDeviceDetailsResponse] = jsonOf[F, BraketDeviceDetailsResponse]
    
            client.run(req).use { resp =>
                resp.status match {
                    case Status.Ok =>
                        resp.asJsonDecode[BraketDeviceDetailsResponse]
                    case other =>
                        resp.as[String].flatMap { body =>
                            Logger[F].info(s"Status: $other, body: $body") *>
                            MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed to fetch device details for $id: $other")
                            )
                        }
                    }
                }
        }

    def getQuantumTask(taskId: String) : F[BraketQuantumTaskResponse] = {
        val region  = "us-east-1"
        val service = "braket"
        val host    = s"$service.$region.amazonaws.com"
        val payload = Array.emptyByteArray
        val path    = AWSSigner.canonicalizePath(taskId)

        val headers = AWSSigner.signRequest(
            method  = Method.GET,
            region  = region,
            service = service,
            host    = host,
            basePath = "/quantum-task",
            rawPath = taskId + "?additionalAttributeNames=QueueInfo",
            payload = payload,
            creds   = cfg
        )

        val req = Request[F](
            method  = Method.GET,
            uri     = Uri.unsafeFromString(s"https://$host/quantum-task/$path?additionalAttributeNames=QueueInfo"),
            headers = Headers(headers.toList.map { case (k, v) => Header.Raw(CaseInsensitiveString(k), v) })
        )

        implicit val userDecoder: EntityDecoder[F, BraketDeviceDetailsResponse] = jsonOf[F, BraketDeviceDetailsResponse]
    
        client.run(req).use { resp =>
                resp.status match {
                    case Status.Ok =>
                        resp.asJsonDecode[BraketQuantumTaskResponse]
                    case other =>
                        resp.as[String].flatMap { body =>
                            Logger[F].info(s"Status: $other, body: $body") *>
                            MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed to fetch device details for $taskId: $other")
                            )
                        }
                    }
                }

    }

    def submitBraketOpenQasmTask(r: BraketCreateQuantumTaskRequest, qasmSource:   String): F[BraketCreateQuantumTaskResponse] = 
        Logger[F].info(s"Submitting Job To Braket") *> 
        ID.make[F, DeviceQueueInformationId].flatMap {id => {
            val oHeader = BraketOpenQasmHeader() // name + version defaults
            val oProg   = BraketOpenQasmProgram(
                braketSchemaHeader = oHeader,
                source             = qasmSource
            )

             val clientToken = id.value.toString

             

            // This must be serialized as a *string* into the action field
             val actionString: String = oProg.asJson.noSpaces

             val reqBody = BraketCreateQuantumTaskRequest(
                action            = actionString,
                associations =    None,
                clientToken       = r.clientToken,
                deviceArn         = r.deviceArn,
                deviceParameters  = "{}",
                None,
                outputS3Bucket    = r.outputS3Bucket,
                outputS3KeyPrefix = r.outputS3KeyPrefix,
                shots             = r.shots,
             )

            val payload    = reqBody.toString.getBytes(StandardCharsets.UTF_8) //  THIS ISNT GOOD

            val host         = s"braket.us-east-1.amazonaws.com"   
            val canonicalUri = "/quantum-task"
            val uri          = Uri.unsafeFromString(s"https://$host$canonicalUri")
            val service = "braket"

            val signed = AWSSigner.signRequest(Method.POST, "us-east-1", service, host, "/quantum-task", "", payload, cfg)
        
            val rawHeaders: List[Header.Raw] =
                signed.toList.map { case (k, v) => Header.Raw(CaseInsensitiveString(k), v) }

            val req = Request[F](
                method  = Method.POST,
                uri     = uri,
                headers = Headers(rawHeaders),
                body    = fs2.Stream.chunk(Chunk.array(payload))
            )

            client.run(req).use { resp =>
                resp.status match {
                    case Status.Ok =>
                    resp.asJsonDecode[BraketCreateQuantumTaskResponse]
                    case other =>
                        resp.as[String].flatMap { body =>
                            Logger[F].info(s"Status: $other, body: $body") *>
                                MonadCancelThrow[F].raiseError(
                                    new Exception(s"Failed to fetch device details for: $other")
                                )
                        }
                }
            }
        }}

    def fetchDeviceCalibration(deviceArn: String): F[DeviceCalibration] = ??? 
    
    private def dumpRequest(label: String, req: Request[F]): F[Unit] =
        Logger[F].info(s"===== $label =====\n" + s"${req.method.name} ${req.uri.renderString}") *> 
            Sync[F].delay {
                req.headers.headers.foreach { h =>
                    val v = if (h.name.toString.equalsIgnoreCase("authorization")){
                                h.value.replaceAll("Signature=[0-9a-fA-F]+", "Signature=<REDACTED>")
                            } 
                            else{h.value}
                    println(s"${h.name}: $v")
                    }
                ()
            }
  }
}