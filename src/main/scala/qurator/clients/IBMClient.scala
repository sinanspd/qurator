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
import qurator.domain.Task.QuantumTask
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.device.Device
import qurator.domain.ProviderClient
import qurator.domain.ProviderJobTiming
import qurator.domain.ProviderTaskStatus
import qurator.domain.QuantumJobResult
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneOffset, ZonedDateTime}
import scala.util.Try

trait IBMClient[F[_]] extends ProviderClient[F] {
  def fetchBearerToken: F[String]
  def fetchDeviceInformation: F[BackendsResponseV2]
  def fetchDeviceDetails(ids: List[String]): F[List[IBMBackendDevice]]
  def submitJob(r: SubmitJobRequestV2): F[CreateJobResponseV2]
  def listJobDetails(id: String): F[JobDetailsResponseV2]
  def getJobMetrics(id: String): F[JobMetricsResponse]
  def getJobResults(id: String): F[String]
  def fetchDeviceCalibration(deviceArn: String): F[DeviceCalibration]

  override final def provider: String =
    "IBM"

  override final def submitTask(
      device: Device,
      task: QuantumTask,
      compiled: Circuit
  ): F[CreateJobResponseV2] = {
    val req =
      SubmitJobRequestV2(
        program_id = "sampler",
        backend = device.platformId,
        runtime = None,
        tags = None,
        log_level = Some("info"),
        cost = None,
        session_id = None,
        calibration_id = None,
        params = SamplerV2Input(
          pubs = List(compiled.toQasm)
        )
      )

    submitJob(req)
  }

  override final def getTask(taskId: String): F[JobDetailsResponseV2] =
    listJobDetails(taskId)

  override final val completedStatuses: Set[String] =
    Set("Completed")
}

object IBMClient {
  private[clients] def parseTimestamp(raw: String): Option[LocalDateTime] = {
    val parsedInstant =
      Try(Instant.parse(raw))
        .orElse(Try(OffsetDateTime.parse(raw).toInstant))
        .orElse(Try(ZonedDateTime.parse(raw).toInstant))
        .toOption

    parsedInstant.map(LocalDateTime.ofInstant(_, ZoneOffset.UTC))
  }

  def jobResultFromRaw(
      provider: String,
      taskId: String,
      status: ProviderTaskStatus,
      raw: String
  ): QuantumJobResult = {
    val deviceId = status match {
      case response: JobDetailsResponseV2 => Some(response.backend)
      case _                              => None
    }

    if (raw.trim.isEmpty)
      QuantumJobResult.unavailable(provider, taskId, deviceId, "IBM returned an empty result payload")
    else
      QuantumJobResult.fromRawText(provider, taskId, deviceId, raw)
  }

  def fetchAvailableDevices[F[_]: cats.Functor](
      fetchDeviceInformation: F[BackendsResponseV2]
  ): F[List[Device]] =
    fetchDeviceInformation.map(_.devices.filter(_.isAvailable).map(_.toDevice))

  def fetchDeviceDetails[F[_]: cats.Functor](
      fetchDeviceInformation: F[BackendsResponseV2],
      ids: List[String]
  ): F[List[IBMBackendDevice]] =
    fetchDeviceInformation.map { response =>
      val requested = ids.toSet
      response.devices.filter(d => requested.isEmpty || requested.contains(d.platformId))
    }

  private def normalizedEdge(a: Int, b: Int): (Int, Int) =
    if (a <= b) (a, b) else (b, a)

  private def gateName(name: String): String =
    name.trim.toUpperCase

  private def average(values: Iterable[Double]): Double = {
    val xs = values.iterator.filter(_.isFinite).toVector
    if (xs.nonEmpty) xs.sum / xs.size else 0.0
  }

  private def averageLong(values: Iterable[Long]): Long = {
    val xs = values.iterator.filter(_ > 0L).toVector
    if (xs.nonEmpty) math.round(xs.sum.toDouble / xs.size.toDouble) else 0L
  }

  private def seconds(value: Double, unit: Option[String]): Double =
    unit.map(_.toLowerCase) match {
      case Some("us") => value / 1e6
      case Some("ms") => value / 1e3
      case Some("ns") => value / 1e9
      case _          => value
    }

  private def durationNs(value: Double, unit: Option[String]): Long =
    unit.map(_.toLowerCase) match {
      case Some("ns") => math.round(value)
      case Some("us") => math.round(value * 1e3)
      case Some("ms") => math.round(value * 1e6)
      case Some("s")  => math.round(value * 1e9)
      case _          => math.round(value)
    }

  private def namedValues(values: List[IBMBackendNamedValue]): Map[String, IBMBackendNamedValue] =
    values.map(v => gateName(v.name) -> v).toMap

  private def buildCalibration(
      properties: IBMBackendPropertiesResponse,
      configuration: IBMBackendConfigurationResponse
  ): IBMCalibration = {
    val qubitMetrics =
      properties.qubits.zipWithIndex.map { case (values, qubit) =>
        val byName = namedValues(values)
        val readoutLength = byName.get("READOUT_LENGTH").map(v => durationNs(v.value, v.unit))
        qubit -> QubitCalibrationMetrics(
          t1Seconds = byName.get("T1").map(v => seconds(v.value, v.unit)),
          t2Seconds = byName.get("T2").map(v => seconds(v.value, v.unit)),
          readoutError = byName.get("READOUT_ERROR").map(_.value),
          probMeasu0Prep1 = byName.get("PROB_MEAS0_PREP1").map(_.value),
          probMeasu1Prep0 = byName.get("PROB_MEAS1_PREP0").map(_.value),
          gateDurationsNs = readoutLength.map("MEASURE" -> _).toMap
        )
      }.toMap

    val withGateMetrics =
      properties.gates.foldLeft(qubitMetrics -> Map.empty[(Int, Int), EdgeCalibrationMetrics]) {
        case ((qAcc, eAcc), gateProps) =>
          val params = namedValues(gateProps.parameters)
          val error = params.get("GATE_ERROR").map(_.value)
          val length = params.get("GATE_LENGTH").map(v => durationNs(v.value, v.unit))
          val gate = gateName(gateProps.gate)

          gateProps.qubits match {
            case q :: Nil =>
              val existing = qAcc.getOrElse(q, QubitCalibrationMetrics())
              val updated = existing.copy(
                gateErrors = existing.gateErrors ++ error.map(gate -> _),
                gateDurationsNs = existing.gateDurationsNs ++ length.map(gate -> _)
              )
              (qAcc.updated(q, updated), eAcc)

            case a :: b :: Nil =>
              val edge = normalizedEdge(a, b)
              val existing = eAcc.getOrElse(edge, EdgeCalibrationMetrics())
              val updated = existing.copy(
                gateErrors = existing.gateErrors ++ error.map(gate -> _),
                gateDurationsNs = existing.gateDurationsNs ++ length.map(gate -> _)
              )
              (qAcc, eAcc.updated(edge, updated))

            case _ =>
              (qAcc, eAcc)
          }
      }

    val enrichedQubits =
      withGateMetrics._1.map { case (qubit, metrics) =>
        val readoutFidelity =
          metrics.probMeasu0Prep1.flatMap { e01 =>
            metrics.probMeasu1Prep0.map(e10 => 1.0 - ((e01 + e10) / 2.0))
          }.orElse(metrics.readoutError.map(e => 1.0 - e))

        qubit -> metrics.copy(readoutFidelity = readoutFidelity)
      }

    val edgeMetrics = withGateMetrics._2

    val edgesFromConfiguration =
      configuration.coupling_map.getOrElse(Nil).collect { case a :: b :: Nil => normalizedEdge(a, b) }

    val edgesFromGateDefinitions =
      configuration.gates.getOrElse(Nil).flatMap(_.coupling_map.collect { case a :: b :: Nil => normalizedEdge(a, b) })

    val edgesFromProperties =
      properties.gates.collect { case gate if gate.qubits.length == 2 => normalizedEdge(gate.qubits.head, gate.qubits(1)) }

    val edges = (edgesFromConfiguration ++ edgesFromGateDefinitions ++ edgesFromProperties).distinct.sorted

    val qubits =
      configuration.n_qubits.map(n => List.range(0, n))
        .getOrElse((properties.qubits.indices.toList ++ edges.flatMap(e => List(e._1, e._2))).distinct.sorted)

    val oneQErrors = enrichedQubits.values.flatMap(_.gateErrors.values)
    val oneQDurations = enrichedQubits.values.flatMap(_.gateDurationsNs.values.filter(_ > 0L))
    val twoQErrors = edgeMetrics.values.flatMap(_.gateErrors.values)
    val twoQDurations = edgeMetrics.values.flatMap(_.gateDurationsNs.values.filter(_ > 0L))

    def avgQubitError(gates: List[String]): Double =
      average(enrichedQubits.values.flatMap(metrics => gates.iterator.flatMap(metrics.gateErrors.get)))

    def avgQubitDuration(gates: List[String]): Long =
      averageLong(enrichedQubits.values.flatMap(metrics => gates.iterator.flatMap(metrics.gateDurationsNs.get)))

    def avgEdgeError(gates: List[String]): Double =
      average(edgeMetrics.values.flatMap(metrics => gates.iterator.flatMap(metrics.gateErrors.get)))

    def avgEdgeDuration(gates: List[String]): Long =
      averageLong(edgeMetrics.values.flatMap(metrics => gates.iterator.flatMap(metrics.gateDurationsNs.get)))

    IBMCalibration(
      qubits = qubits,
      edges = edges,
      t1Seconds = enrichedQubits.toList.flatMap { case (q, metrics) => metrics.t1Seconds.map(q -> _) }.sortBy(_._1),
      t2Seconds = enrichedQubits.toList.flatMap { case (q, metrics) => metrics.t2Seconds.map(q -> _) }.sortBy(_._1),
      t1AvgSeconds = average(enrichedQubits.values.flatMap(_.t1Seconds)),
      t2AvgSeconds = average(enrichedQubits.values.flatMap(_.t2Seconds)),
      probMeasu0Presp1 = average(enrichedQubits.values.flatMap(_.probMeasu0Prep1)),
      probMeasu1Presp0 = average(enrichedQubits.values.flatMap(_.probMeasu1Prep0)),
      idError = avgQubitError(List("ID")),
      rxError = avgQubitError(List("RX", "SX", "X")),
      pauliXError = avgQubitError(List("X")),
      czError = avgEdgeError(List("CZ", "ECR", "CX")),
      rzzError = avgEdgeError(List("RZZ")),
      readoutLengthNs = avgQubitDuration(List("MEASURE")),
      singleQGateLengthNs = avgQubitDuration(List("RX", "SX", "X")),
      idLengthNs = avgQubitDuration(List("ID")),
      twoQGateLengthNs = avgEdgeDuration(List("CX", "ECR", "CZ")),
      czGateLengthNs = avgEdgeDuration(List("CZ", "ECR", "CX")),
      rzzGateLengthNs = avgEdgeDuration(List("RZZ")),
      topology = Some(CalibrationTopology(qubits, edges)),
      qubitMetrics = enrichedQubits,
      edgeMetrics = edgeMetrics,
      basisGates = configuration.basis_gates.getOrElse(Nil).map(gateName),
      calibrationId = None,
      updatedAt = properties.last_update_date
    )
  }

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
              case _ =>  MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed")
                            )
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
                case _ =>  MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed")
                            )
              }
            }
          }
        }

      def fetchAvailableDevices: F[List[Device]] =
        IBMClient.fetchAvailableDevices(fetchDeviceInformation)

      def fetchDeviceDetails(ids: List[String]): F[List[IBMBackendDevice]] =
        IBMClient.fetchDeviceDetails(fetchDeviceInformation, ids)

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
                 case _ =>  MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed")
                            )
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
               case _ =>  MonadCancelThrow[F].raiseError(
                                new Exception(s"Failed")
                            )
            }
          }
        }
      }

    def getJobResults(id: String): F[String] =
      fetchBearerToken.flatMap { token =>
        Uri.fromString(s"https://quantum.cloud.ibm.com/api/v1/jobs/$id/results").liftTo[F].flatMap { uri =>
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
                resp.as[String]
              case Status.NoContent =>
                "".pure[F]
              case other =>
                resp.as[String].flatMap { body =>
                  Logger[F].info(s"Status: $other, body: $body") *>
                    MonadCancelThrow[F].raiseError(
                      new Exception(s"Failed to fetch IBM job results for $id: $other")
                    )
                }
            }
          }
        }
      }

    def fetchTaskResult(taskId: String, status: ProviderTaskStatus): F[QuantumJobResult] =
      getJobResults(taskId).map(raw => IBMClient.jobResultFromRaw(provider, taskId, status, raw))

    def fetchJobTiming(taskId: String, status: ProviderTaskStatus): F[ProviderJobTiming] =
      getJobMetrics(taskId).map { metrics =>
        ProviderJobTiming(
          startedAt = metrics.timestamps.running.flatMap(IBMClient.parseTimestamp),
          completedAt = metrics.timestamps.finished.flatMap(IBMClient.parseTimestamp)
        )
      }

    private def fetchBackendProperties(
        backendId: String,
        token: String
    ): F[IBMBackendPropertiesResponse] =
      Uri.fromString(s"https://quantum.cloud.ibm.com/api/v1/backends/$backendId/properties").liftTo[F].flatMap { uri =>
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
              resp.asJsonDecode[IBMBackendPropertiesResponse]
            case other =>
              resp.as[String].flatMap { body =>
                Logger[F].info(s"Status: $other, body: $body") *>
                  MonadCancelThrow[F].raiseError(
                    new Exception(s"Failed to fetch IBM backend properties for $backendId: $other")
                  )
              }
          }
        }
      }

    private def fetchBackendConfiguration(
        backendId: String,
        token: String
    ): F[IBMBackendConfigurationResponse] =
      Uri.fromString(s"https://quantum.cloud.ibm.com/api/v1/backends/$backendId/configuration").liftTo[F].flatMap { uri =>
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
              resp.asJsonDecode[IBMBackendConfigurationResponse]
            case other =>
              resp.as[String].flatMap { body =>
                Logger[F].info(s"Status: $other, body: $body") *>
                  MonadCancelThrow[F].raiseError(
                    new Exception(s"Failed to fetch IBM backend configuration for $backendId: $other")
                  )
              }
          }
        }
      }

    def fetchDeviceCalibration(deviceArn: String): F[DeviceCalibration] =
      fetchBearerToken.flatMap { token =>
        (
          fetchBackendProperties(deviceArn, token),
          fetchBackendConfiguration(deviceArn, token)
        ).mapN(IBMClient.buildCalibration)
      }
}}
