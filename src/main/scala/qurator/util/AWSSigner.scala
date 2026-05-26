package qurator.util

import cats.effect.Sync
import org.http4s.Method
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.time.{ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.HexFormat
import qurator.domain.Braket.BraketConfig
import ciris.Secret
import eu.timepit.refined.types.string.NonEmptyString
import org.http4s.Uri

object AWSSigner {

  private val Algorithm = "AWS4-HMAC-SHA256"
  private val HmacAlgo  = "HmacSHA256"
  private val hex       = HexFormat.of()

  private val amzDateFmt = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
  private val ymdFmt     = DateTimeFormatter.ofPattern("yyyyMMdd")

  private def sha256Hex(bytes: Array[Byte]): String =
    hex.formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))

  private def hmac(key: Array[Byte], data: String): Array[Byte] = {
    val mac = Mac.getInstance(HmacAlgo)
    mac.init(new SecretKeySpec(key, HmacAlgo))
    mac.doFinal(data.getBytes(StandardCharsets.UTF_8))
  }

  private def deriveSigningKey(
    secret: Secret[NonEmptyString],
    date:   String,
    region: String,
    service:String
  ): Array[Byte] = {
    val kDate   = hmac(("AWS4" + secret.value).getBytes(StandardCharsets.UTF_8), date)
    val kRegion = hmac(kDate, region)
    val kSvc    = hmac(kRegion, service)
    hmac(kSvc, "aws4_request")
  }


  def canonicalizePath(path: String): String = {
    val sb = new StringBuilder(path.length * 2)
    path.foreach { ch =>
      val unreserved =
        (ch >= 'A' && ch <= 'Z') ||
        (ch >= 'a' && ch <= 'z') ||
        (ch >= '0' && ch <= '9') ||
        ch == '-' || ch == '_' || ch == '.' || ch == '~'

      if (unreserved) sb.append(ch)
      else {
        val bytes = ch.toString.getBytes(StandardCharsets.UTF_8)
        bytes.foreach { b =>
          sb.append("%%%02X".format(b & 0xff))
        }
      }
    }
    sb.toString()
  }
  
  def signRequest(
  method:  Method,
  region:  String,
  service: String,
  host:    String,
  basePath: String,
  rawPath: String,            
  payload: Array[Byte],
  creds:   BraketConfig,
  includePayloadHashHeader: Boolean = false
): Map[String, String] = {

    val nowUtc    = ZonedDateTime.now(ZoneOffset.UTC)
    val amzDate   = amzDateFmt.format(nowUtc)
    val dateStamp = ymdFmt.format(nowUtc)

    val payloadHash = payload 

    // In a momentary lapse of all reason, Amazon decided that ARNs should be double encoded
    val canonicalPath  = if(rawPath != ""){basePath + "/" + canonicalizePath(canonicalizePath(rawPath))}else{basePath}
    val canonicalQuery = ""                        //Not used but still need to be signed unfortunately

    val baseHeaders = List(
      "host"                 -> host,
      "x-amz-date"           -> amzDate
    )

    val headers =
      (if (includePayloadHashHeader) baseHeaders :+ ("x-amz-content-sha256" -> sha256Hex(payloadHash)) else baseHeaders)
        .sortBy(_._1)

    val canonicalHeaders =
      headers.map { case (k, v) => s"$k:${v.trim}\n" }.mkString

    val signedHeaders =
      headers.map(_._1).mkString(";")

    val canonicalRequest =
      s"${method.name}\n$canonicalPath\n$canonicalQuery\n$canonicalHeaders\n$signedHeaders\n${sha256Hex(payloadHash)}"

    val canonicalRequestHash =
      sha256Hex(canonicalRequest.getBytes(StandardCharsets.UTF_8))

    val scope       = s"$dateStamp/$region/$service/aws4_request"
    val stringToSign =
      s"$Algorithm\n$amzDate\n$scope\n$canonicalRequestHash"

    // println(s"Canonical Request:\n$canonicalRequest\n")
    // println(s"String to Sign:\n$stringToSign\n")

    val signingKey = deriveSigningKey(creds.apiSecret, dateStamp, region, service)
    val signature  = hex.formatHex(hmac(signingKey, stringToSign))

    val authorization =
      s"$Algorithm Credential=${creds.accessId}/$scope, SignedHeaders=$signedHeaders, Signature=$signature"

    val signedHeadersMap = Map(
      "host"                 -> host,
      "authorization"        -> authorization,
      "x-amz-date"           -> amzDate
    )

    if (includePayloadHashHeader)
      signedHeadersMap + ("x-amz-content-sha256" -> sha256Hex(payloadHash))
    else signedHeadersMap
  }
}
