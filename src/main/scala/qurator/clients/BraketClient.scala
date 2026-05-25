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
import io.circe.{HCursor, Json}
import io.circe.syntax._ 
import io.circe.parser.parse
import java.util.UUID
import qurator.domain.Task.QuantumTask
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.device.Device
import qurator.domain.ProviderClient
import qurator.domain.ProviderJobTiming
import qurator.domain.ProviderTaskStatus
import scala.util.Try
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneOffset, ZonedDateTime}

trait BraketClient[F[_]] extends ProviderClient[F] {
  def fetchAvailableDevices: F[List[Device]]
  def fetchDeviceList: F[BraketDeviceListResponse]
  def fetchDeviceDetails(ids: List[String]): F[List[BraketDeviceDetailsResponse]]
  def submitBraketOpenQasmTask(r: BraketCreateQuantumTaskRequest, qasmSource:   String): F[BraketCreateQuantumTaskResponse] 
  def getQuantumTask(taskId: String) : F[BraketQuantumTaskResponse]
  def fetchDeviceCalibration(deviceArn: String): F[DeviceCalibration]

  override final def provider: String =
    "Braket"

  override final def submitTask(
      device: Device,
      task: QuantumTask,
      compiled: Circuit
  ): F[BraketCreateQuantumTaskResponse] = {
    val req =
      BraketCreateQuantumTaskRequest(
        action = "braket.ir.openqasm.program",
        associations = None,
        clientToken = UUID.randomUUID().toString,
        deviceArn = device.platformId,
        deviceParameters = "{}",
        shots = task.shots.value
      )

    submitBraketOpenQasmTask(req, compiled.toQasm)
  }

  override final def getTask(taskId: String): F[BraketQuantumTaskResponse] =
    getQuantumTask(taskId)

  override final val completedStatuses: Set[String] =
    Set("COMPLETED")
}
        

object BraketClient {
  private[clients] def parseTimestamp(raw: String): Option[LocalDateTime] = {
    val parsedInstant =
      Try(Instant.parse(raw))
        .orElse(Try(OffsetDateTime.parse(raw).toInstant))
        .orElse(Try(ZonedDateTime.parse(raw).toInstant))
        .toOption

    parsedInstant.map(LocalDateTime.ofInstant(_, ZoneOffset.UTC))
  }

  def fetchAvailableDevices[F[_]: cats.Monad](
      fetchDeviceList: F[BraketDeviceListResponse],
      fetchDeviceDetails: List[String] => F[List[BraketDeviceDetailsResponse]]
  ): F[List[Device]] =
    fetchDeviceList
      .flatMap(resp => fetchDeviceDetails(resp.availableDeviceIds))
      .map(_.map(_.toDevice))

  private val PreferredOneQubitFidelityTypes: List[String] =
    List(
      "SIMULTANEOUS_RANDOMIZED_BENCHMARKING",
      "RANDOMIZED_BENCHMARKING"
    )

  private val PreferredTwoQubitFidelityTypes: List[String] =
    List(
      "SIMULTANEOUS_INTERLEAVED_RANDOMIZED_BENCHMARKING",
      "INTERLEAVED_RANDOMIZED_BENCHMARKING",
      "SIMULTANEOUS_RANDOMIZED_BENCHMARKING",
      "INTERLEAVED_RANDOMIZED_BENCHMARKING",
      "RANDOMIZED_BENCHMARKING"
    )

  private val IQMPreferredTwoQubitGates: List[String] =
    List("CZ")

  private val RigettiPreferredTwoQubitGates: List[String] =
    List("ISWAP", "XY", "CZ", "CPHASESHIFT", "CNOT")

  private def regionForArn(arn: String): String =
    arn.split(":").lift(3).filter(_.nonEmpty).getOrElse("us-east-1")

  private def normalizedEdge(a: Int, b: Int): (Int, Int) =
    if (a <= b) (a, b) else (b, a)

  private def asDouble(json: Json): Option[Double] =
    json.asNumber.map(_.toDouble).orElse(json.asString.flatMap(_.toDoubleOption))

  private def asInt(json: Json): Option[Int] =
    json.asNumber.flatMap(_.toInt).orElse(json.asString.flatMap(_.toIntOption))

  private def objectFields(json: Json): List[(String, Json)] =
    json.asObject.map(_.toList).getOrElse(Nil)

  private def average(values: Iterable[Double]): Double = {
    val xs = values.iterator.filter(_.isFinite).toVector
    if (xs.nonEmpty) xs.sum / xs.size else Double.NaN
  }

  private def averageOption(values: Iterable[Double]): Option[Double] = {
    val xs = values.iterator.filter(_.isFinite).toVector
    if (xs.nonEmpty) Some(xs.sum / xs.size) else None
  }

  private def toPct(value: Double): Double =
    if (value.isNaN) Double.NaN else if (value <= 1.0) value * 100.0 else value

  private def schemaName(json: Json): Option[String] =
    json.hcursor.downField("braketSchemaHeader").get[String]("name").toOption

  private def pathValue(root: Json, path: String): Option[Json] =
    path
      .split('.')
      .filter(_.nonEmpty)
      .foldLeft(Option(root)) { case (json, field) => json.flatMap(_.hcursor.downField(field).focus) }

  private def pathDouble(root: Json, path: String): Option[Double] =
    pathValue(root, path).flatMap(asDouble)

  private def durationSeconds(json: Json): Option[Double] = {
    val c = json.hcursor
    asDouble(json) orElse {
      for {
        value <- c.get[Double]("value").toOption
        unit = c.get[String]("unit").toOption.getOrElse("s").toLowerCase
      } yield {
        unit match {
          case "ns"  => value / 1e9
          case "us"  => value / 1e6
          case "ms"  => value / 1e3
          case "s"   => value
          case "sec" => value
          case _     => value
        }
      }
    }
  }

  private def valueSeconds(root: Json, field: String): Option[Double] =
    root.hcursor.downField(field).focus.flatMap(durationSeconds)

  private def findFidelity(
      entries: List[Json],
      fidelityTypes: List[String] = Nil,
      gateNames: List[String] = Nil
  ): Option[Double] = {
    def matches(entry: Json, useGate: Boolean, useType: Boolean): Boolean = {
      val c = entry.hcursor
      val gateOk =
        !useGate || gateNames.isEmpty || c.get[String]("gateName").toOption.exists(g => gateNames.contains(g))
      val typeOk =
        !useType || fidelityTypes.isEmpty || c.downField("fidelityType").get[String]("name").toOption.exists(t => fidelityTypes.contains(t))
      gateOk && typeOk
    }

    def first(useGate: Boolean, useType: Boolean): Option[Double] =
      entries.collectFirst(Function.unlift { entry =>
        if (matches(entry, useGate, useType)) entry.hcursor.get[Double]("fidelity").toOption else None
      })

    first(useGate = true, useType = true)
      .orElse(first(useGate = true, useType = false))
      .orElse(first(useGate = false, useType = true))
      .orElse(entries.collectFirst(Function.unlift(_.hcursor.get[Double]("fidelity").toOption)))
  }

  private def parseEdgeKey(raw: String): Option[(Int, Int)] = {
    val nums = "-?\\d+".r.findAllIn(raw).toList.flatMap(_.toIntOption)
    nums match {
      case a :: b :: _ => Some(normalizedEdge(a, b))
      case _           => None
    }
  }

  private def parseStandardizedMetrics(standardized: Json): (Map[Int, QubitCalibrationMetrics], Map[(Int, Int), EdgeCalibrationMetrics]) = {
    val oneQubit =
      standardized.hcursor.downField("oneQubitProperties").focus.toList.flatMap(objectFields).flatMap {
        case (rawQubit, props) =>
          rawQubit.toIntOption.map { qubit =>
            val c = props.hcursor
            val fidelityEntries = c.downField("oneQubitFidelity").focus.flatMap(_.asArray).map(_.toList).getOrElse(Nil)
            val readoutFidelity =
              findFidelity(fidelityEntries, fidelityTypes = List("READOUT"))
                .orElse {
                  for {
                    e01 <- findFidelity(fidelityEntries, fidelityTypes = List("READOUT_ERROR_0_TO_1"))
                    e10 <- findFidelity(fidelityEntries, fidelityTypes = List("READOUT_ERROR_1_TO_0"))
                  } yield 1.0 - ((e01 + e10) / 2.0)
                }

            qubit -> QubitCalibrationMetrics(
              t1Seconds = c.downField("T1").focus.flatMap(durationSeconds),
              t2Seconds = c.downField("T2").focus.flatMap(durationSeconds),
              oneQubitFidelity = findFidelity(fidelityEntries, fidelityTypes = PreferredOneQubitFidelityTypes),
              readoutFidelity = readoutFidelity,
              probMeasu0Prep1 = findFidelity(fidelityEntries, fidelityTypes = List("READOUT_ERROR_0_TO_1")),
              probMeasu1Prep0 = findFidelity(fidelityEntries, fidelityTypes = List("READOUT_ERROR_1_TO_0"))
            )
          }
      }.toMap

    val twoQubit =
      standardized.hcursor.downField("twoQubitProperties").focus.toList.flatMap(objectFields).flatMap {
        case (rawEdge, props) =>
          parseEdgeKey(rawEdge).map { edge =>
            val fidelities =
              props.hcursor.downField("twoQubitGateFidelity").focus.flatMap(_.asArray).map(_.toList).getOrElse(Nil)
                .flatMap { entry =>
                  for {
                    gate <- entry.hcursor.get[String]("gateName").toOption
                    fidelity <- entry.hcursor.get[Double]("fidelity").toOption
                  } yield gate.toUpperCase -> fidelity
                }.toMap

            edge -> EdgeCalibrationMetrics(
              gateFidelities = fidelities
            )
          }
      }.toMap

    (oneQubit, twoQubit)
  }

  private def preferredGateFidelity(metrics: EdgeCalibrationMetrics, preferredGateNames: List[String]): Option[Double] = {
    val upperPreferred = preferredGateNames.map(_.toUpperCase)
    upperPreferred.iterator.flatMap(name => metrics.gateFidelities.get(name)).toSeq.headOption
      .orElse(metrics.gateFidelities.values.headOption)
  }

  private def extractTopology(root: Json): Option[CalibrationTopology] = {
    val paradigm = root.hcursor.downField("paradigm").focus.getOrElse(root)
    val pc = paradigm.hcursor
    val qubitCount =
      pc.get[Int]("qubitCount").toOption
        .orElse(pc.get[Int]("modes").toOption)

    val connectivity = pc.downField("connectivity")
    val fullyConnected = connectivity.get[Boolean]("fullyConnected").toOption.getOrElse(false)

    val graphEdges =
      connectivity.downField("connectivityGraph").focus.toList.flatMap(objectFields).flatMap {
        case (src, targetsJson) =>
          src.toIntOption.toList.flatMap { source =>
            targetsJson.asArray.map(_.toList).getOrElse(Nil).flatMap(asInt).map(target => normalizedEdge(source, target)) :::
              objectFields(targetsJson).flatMap { case (target, enabledJson) =>
                val enabled = enabledJson.asBoolean.getOrElse(true)
                if (enabled) target.toIntOption.map(t => normalizedEdge(source, t)).toList else Nil
              }
          }
      }.distinct.sorted

    val inferredEdges =
      if (graphEdges.nonEmpty) graphEdges
      else if (fullyConnected) {
        qubitCount.toList.flatMap { count =>
          (0 until count).toList.combinations(2).collect { case List(a, b) => (a, b) }.toList
        }
      } else Nil

    val qubitsFromEdges = inferredEdges.flatMap { case (a, b) => List(a, b) }
    val qubits =
      qubitCount.map(count => List.range(0, count))
        .orElse {
          val qs = qubitsFromEdges.distinct.sorted
          if (qs.nonEmpty) Some(qs) else None
        }

    qubits.map(qs => CalibrationTopology(qs, inferredEdges))
  }

  private def inferTopologyFromStandardized(standardized: Json): Option[CalibrationTopology] = {
    val qubits =
      standardized.hcursor.downField("oneQubitProperties").focus.toList.flatMap(objectFields).flatMap(_._1.toIntOption).distinct.sorted
    val edges =
      standardized.hcursor.downField("twoQubitProperties").focus.toList.flatMap(objectFields).flatMap { case (rawEdge, _) =>
        parseEdgeKey(rawEdge)
      }.distinct.sorted

    if (qubits.nonEmpty || edges.nonEmpty) Some(CalibrationTopology((qubits ++ edges.flatMap(e => List(e._1, e._2))).distinct.sorted, edges))
    else None
  }

  private def standardizedPayload(root: Json): Option[Json] =
    root.hcursor.downField("standardized").focus
      .orElse(root.hcursor.downField("provider").downField("standardized").focus)
      .orElse(schemaName(root).filter(_ == "braket.device_schema.standardized_gate_model_qpu_device_properties").map(_ => root))

  private def queraPayload(root: Json): Option[Json] =
    root.hcursor.downField("paradigm").focus
      .filter(json => schemaName(json).contains("braket.device_schema.quera.quera_ahs_paradigm_properties"))
      .orElse {
        schemaName(root).filter(_ == "braket.device_schema.quera.quera_ahs_paradigm_properties").map(_ => root)
      }

  private def parseAQTCalibration(standardized: Json, topology: Option[CalibrationTopology]): AQTCalibration = {
    val (qubitMetrics, edgeMetrics) = parseStandardizedMetrics(standardized)
    val c = standardized.hcursor
    val oneQAvg = c.downField("singleQubitFidelity").focus.flatMap(_.asArray).map(_.toList).flatMap(findFidelity(_, fidelityTypes = PreferredOneQubitFidelityTypes))
      .orElse(averageOption(qubitMetrics.values.flatMap(_.oneQubitFidelity)))
      .getOrElse(Double.NaN)
    val twoQAvg = c.downField("twoQubitGateFidelity").focus.flatMap(_.asArray).map(_.toList).flatMap(findFidelity(_, fidelityTypes = PreferredTwoQubitFidelityTypes))
      .getOrElse(Double.NaN)
    val readoutAvg = c.downField("readoutFidelity").focus.flatMap(_.asArray).map(_.toList).flatMap(findFidelity(_))
      .orElse(averageOption(qubitMetrics.values.flatMap(_.readoutFidelity)))
      .getOrElse(Double.NaN)

    AQTCalibration(
      t1Seconds = valueSeconds(standardized, "T1").getOrElse(Double.NaN),
      t2Seconds = valueSeconds(standardized, "T2").getOrElse(Double.NaN),
      readoutFidelity = toPct(readoutAvg),
      readoutDurationSec = valueSeconds(standardized, "readoutDuration").getOrElse(Double.NaN),
      oneQGateDurationSec = valueSeconds(standardized, "singleQubitGateDuration").getOrElse(Double.NaN),
      oneQGateFidelity = toPct(oneQAvg),
      twoQGateDurationSec = valueSeconds(standardized, "twoQubitGateDuration").getOrElse(Double.NaN),
      twoQGateFidelity = toPct(twoQAvg),
      topology = topology,
      qubitMetrics = qubitMetrics,
      edgeMetrics = edgeMetrics,
      updatedAt = c.get[String]("updatedAt").toOption
    )
  }

  private def parseAQTLegacy(root: Json, topology: Option[CalibrationTopology]): AQTCalibration = {
    val provider = root.hcursor.downField("provider").focus.getOrElse(root)

    AQTCalibration(
      t1Seconds = pathDouble(provider, "t1Seconds").getOrElse(Double.NaN),
      t2Seconds = pathDouble(provider, "t2Seconds").getOrElse(Double.NaN),
      readoutFidelity = toPct(
        pathDouble(provider, "fidelity.readout")
          .orElse(pathDouble(provider, "readoutFidelity"))
          .getOrElse(Double.NaN)
      ),
      readoutDurationSec = pathDouble(provider, "readoutDurationSec").getOrElse(Double.NaN),
      oneQGateDurationSec = pathDouble(provider, "oneQGateDurationSec").getOrElse(Double.NaN),
      oneQGateFidelity = toPct(
        pathDouble(provider, "fidelity.oneQubit")
          .orElse(pathDouble(provider, "oneQGateFidelity"))
          .getOrElse(Double.NaN)
      ),
      twoQGateDurationSec = pathDouble(provider, "twoQGateDurationSec").getOrElse(Double.NaN),
      twoQGateFidelity = toPct(
        pathDouble(provider, "fidelity.twoQubit")
          .orElse(pathDouble(provider, "twoQGateFidelity"))
          .getOrElse(Double.NaN)
      ),
      topology = topology
    )
  }

  private def parseIonQStandardized(standardized: Json, topology: Option[CalibrationTopology]): IonQCalibration = {
    val c = standardized.hcursor
    val oneQAvg = c.downField("singleQubitFidelity").focus.flatMap(_.asArray).map(_.toList).flatMap(findFidelity(_, fidelityTypes = PreferredOneQubitFidelityTypes)).getOrElse(Double.NaN)
    val twoQAvg = c.downField("twoQubitGateFidelity").focus.flatMap(_.asArray).map(_.toList).flatMap(findFidelity(_, fidelityTypes = PreferredTwoQubitFidelityTypes)).getOrElse(Double.NaN)
    val readoutAvg = c.downField("readoutFidelity").focus.flatMap(_.asArray).map(_.toList).flatMap(findFidelity(_)).getOrElse(Double.NaN)

    IonQCalibration(
      t1Seconds = valueSeconds(standardized, "T1").getOrElse(Double.NaN),
      t2Seconds = valueSeconds(standardized, "T2").getOrElse(Double.NaN),
      avg1qFidelityPct = toPct(oneQAvg),
      avg2qFidelityPct = toPct(twoQAvg),
      avgReadoutFidelity = toPct(readoutAvg),
      oneQGateDurationSec = valueSeconds(standardized, "singleQubitGateDuration").getOrElse(Double.NaN),
      twoQGateDurationSec = valueSeconds(standardized, "twoQubitGateDuration").getOrElse(Double.NaN),
      readoutDurationSec = valueSeconds(standardized, "readoutDuration").getOrElse(Double.NaN),
      topology = topology,
      updatedAt = c.get[String]("updatedAt").toOption
    )
  }

  private def parseIonQLegacy(root: Json, topology: Option[CalibrationTopology]): IonQCalibration = {
    val provider = root.hcursor.downField("provider").focus.getOrElse(root)

    IonQCalibration(
      t1Seconds = pathDouble(provider, "timing.T1").getOrElse(Double.NaN),
      t2Seconds = pathDouble(provider, "timing.T2").getOrElse(Double.NaN),
      avg1qFidelityPct = toPct(pathDouble(provider, "fidelity.1Q.mean").getOrElse(Double.NaN)),
      avg2qFidelityPct = toPct(pathDouble(provider, "fidelity.2Q.mean").getOrElse(Double.NaN)),
      avgReadoutFidelity = toPct(pathDouble(provider, "fidelity.spam.mean").getOrElse(Double.NaN)),
      oneQGateDurationSec = pathDouble(provider, "timing.1Q").getOrElse(Double.NaN),
      twoQGateDurationSec = pathDouble(provider, "timing.2Q").getOrElse(Double.NaN),
      readoutDurationSec = pathDouble(provider, "timing.readout").getOrElse(Double.NaN),
      topology = topology
    )
  }

  private def parseIQMCalibration(standardized: Json, topology: Option[CalibrationTopology]): IQMCalibration = {
    val (qubitMetrics, edgeMetrics) = parseStandardizedMetrics(standardized)

    IQMCalibration(
      t1 = average(qubitMetrics.values.flatMap(_.t1Seconds)),
      t2 = average(qubitMetrics.values.flatMap(_.t2Seconds)),
      q1fidelity = toPct(average(qubitMetrics.values.flatMap(_.oneQubitFidelity))),
      q2fidelity = toPct(average(edgeMetrics.values.flatMap(metrics => preferredGateFidelity(metrics, IQMPreferredTwoQubitGates)))),
      readoutFidelity = toPct(average(qubitMetrics.values.flatMap(_.readoutFidelity))),
      topology = topology.orElse(inferTopologyFromStandardized(standardized)),
      qubitMetrics = qubitMetrics,
      edgeMetrics = edgeMetrics,
      updatedAt = standardized.hcursor.get[String]("updatedAt").toOption
    )
  }

  private def parseRigettiCalibration(standardized: Json, topology: Option[CalibrationTopology]): RigettiCalibration = {
    val (qubitMetrics, edgeMetrics) = parseStandardizedMetrics(standardized)

    RigettiCalibration(
      t1Seconds = average(qubitMetrics.values.flatMap(_.t1Seconds)),
      t2Seconds = average(qubitMetrics.values.flatMap(_.t2Seconds)),
      avg1qFidelityPct = toPct(average(qubitMetrics.values.flatMap(_.oneQubitFidelity))),
      readoutFidelityPct = toPct(average(qubitMetrics.values.flatMap(_.readoutFidelity))),
      swapFidelityPct = toPct(average(edgeMetrics.values.flatMap(metrics => preferredGateFidelity(metrics, RigettiPreferredTwoQubitGates)))),
      oneQGateDurationNs = 0L,
      twoQGateDurationNs = 0L,
      swapGateDurationNs = 0L,
      readoutDurationNs = 0L,
      topology = topology.orElse(inferTopologyFromStandardized(standardized)),
      qubitMetrics = qubitMetrics,
      edgeMetrics = edgeMetrics,
      updatedAt = standardized.hcursor.get[String]("updatedAt").toOption
    )
  }

  private def parseQuEraCalibration(paradigm: Json): QuEraCalibration = {
    val c = paradigm.hcursor
    QuEraCalibration(
      typicalDetectionFalsePositive = pathDouble(paradigm, "performance.lattice.atomDetectionErrorFalsePositiveTypical").getOrElse(Double.NaN),
      typicalDetectionFalseNegative = pathDouble(paradigm, "performance.lattice.atomDetectionErrorFalseNegativeTypical").getOrElse(Double.NaN),
      typicalVacancyError = pathDouble(paradigm, "performance.lattice.vacancyErrorTypical"),
      typicalFillingError = pathDouble(paradigm, "performance.lattice.fillingErrorTypical"),
      typicalAtomLossProbability = pathDouble(paradigm, "performance.lattice.atomLossProbabilityTypical"),
      t1SingleSec = pathDouble(paradigm, "performance.rydberg.rydbergGlobal.T1Single"),
      t2EchoSingleSec = pathDouble(paradigm, "performance.rydberg.rydbergGlobal.T2EchoSingle"),
      t2SingleSec = pathDouble(paradigm, "performance.rydberg.rydbergGlobal.T2StarSingle")
    )
  }

  def parseCalibration(deviceArn: String, providerName: String, deviceCapabilities: String): Either[String, DeviceCalibration] =
    parse(deviceCapabilities)
      .leftMap(_.message)
      .flatMap { root =>
        val topology = extractTopology(root)
        val provider = providerName.toLowerCase

        if (provider.contains("quera")) {
          queraPayload(root)
            .map(parseQuEraCalibration)
            .toRight(s"No QuEra paradigm payload found in capabilities for $deviceArn")
        } else {
          val standardized = standardizedPayload(root).orElse {
            schemaName(root).filter(_ == "braket.device_schema.standardized_gate_model_qpu_device_properties").map(_ => root)
          }

          provider match {
            case p if p.contains("aqt") =>
              standardized.map(parseAQTCalibration(_, topology.orElse(standardized.flatMap(inferTopologyFromStandardized))))
                .orElse(Some(parseAQTLegacy(root, topology)))
                .toRight(s"No AQT calibration payload found in capabilities for $deviceArn")

            case p if p.contains("ionq") =>
              standardized.map(parseIonQStandardized(_, topology.orElse(standardized.flatMap(inferTopologyFromStandardized))))
                .orElse(Some(parseIonQLegacy(root, topology)))
                .toRight(s"No IonQ calibration payload found in capabilities for $deviceArn")

            case p if p.contains("iqm") =>
              standardized.map(parseIQMCalibration(_, topology.orElse(standardized.flatMap(inferTopologyFromStandardized))))
                .toRight(s"No IQM standardized calibration payload found in capabilities for $deviceArn")

            case p if p.contains("rigetti") =>
              standardized.map(parseRigettiCalibration(_, topology.orElse(standardized.flatMap(inferTopologyFromStandardized))))
                .toRight(s"No Rigetti standardized calibration payload found in capabilities for $deviceArn")

            case other =>
              Left(s"Unsupported Braket provider for calibration parsing: $other")
          }
        }
      }

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
            val region  = regionForArn(id)
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

    def fetchAvailableDevices: F[List[Device]] =
        BraketClient.fetchAvailableDevices(fetchDeviceList, fetchDeviceDetails)

    def getQuantumTask(taskId: String) : F[BraketQuantumTaskResponse] = {
        val region  = regionForArn(taskId)
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

    def fetchJobTiming(taskId: String, status: ProviderTaskStatus): F[ProviderJobTiming] =
        status match {
            case response: BraketQuantumTaskResponse =>
                ProviderJobTiming(
                    startedAt = response.startedAt.flatMap(BraketClient.parseTimestamp),
                    completedAt = response.endedAt.flatMap(BraketClient.parseTimestamp)
                ).pure[F]

            case _ =>
                ProviderJobTiming(None, None).pure[F]
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

            val payload    = reqBody.toString.getBytes(StandardCharsets.UTF_8)

            val region       = regionForArn(r.deviceArn)
            val host         = s"braket.$region.amazonaws.com"   
            val canonicalUri = "/quantum-task"
            val uri          = Uri.unsafeFromString(s"https://$host$canonicalUri")
            val service = "braket"

            val signed = AWSSigner.signRequest(Method.POST, region, service, host, "/quantum-task", "", payload, cfg)
        
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

    def fetchDeviceCalibration(deviceArn: String): F[DeviceCalibration] =
        fetchDeviceDetails(List(deviceArn)).flatMap {
            case detail :: _ =>
                BraketClient
                    .parseCalibration(detail.deviceArn, detail.providerName, detail.deviceCapabilities)
                    .leftMap(new Exception(_))
                    .liftTo[F]
            case Nil =>
                MonadCancelThrow[F].raiseError(new Exception(s"Failed to fetch Braket device details for $deviceArn"))
        }
    
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
