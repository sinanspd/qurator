package qurator.domain

import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import qurator.domain.Task.TaskId
import qurator.domain.circuit.Circuit

final case class QuantumResultValue(
    name: String,
    targets: List[Int],
    value: Json
)

final case class QuantumJobResult(
    provider: String,
    jobId: String,
    deviceId: Option[String],
    shots: Option[Int],
    counts: Map[String, Long],
    probabilities: Map[String, Double],
    measurements: Option[List[List[Int]]],
    resultTypes: List[QuantumResultValue],
    raw: Json,
    taskId: Option[TaskId] = None,
    executedCircuit: Option[Circuit] = None
) {
    def summary: String =
        raw.hcursor.downField("unavailable").as[String].toOption match {
            case Some(reason) => s"quantum result unavailable: $reason"
            case None if counts.nonEmpty =>
                s"counts=${counts.toList.sortBy(_._1).map { case (k, v) => s"$k:$v" }.mkString("{", ",", "}")}"
            case None if probabilities.nonEmpty =>
                s"probabilities=${probabilities.toList.sortBy(_._1).map { case (k, v) => s"$k:$v" }.mkString("{", ",", "}")}"
            case None if resultTypes.nonEmpty =>
                s"resultTypes=${resultTypes.map(_.name).mkString("[", ",", "]")}"
            case None =>
                "quantum result available"
        }
}

object QuantumJobResult {
    def unavailable(provider: String, jobId: String, deviceId: Option[String], reason: String): QuantumJobResult =
        QuantumJobResult(
            provider = provider,
            jobId = jobId,
            deviceId = deviceId,
            shots = None,
            counts = Map.empty,
            probabilities = Map.empty,
            measurements = None,
            resultTypes = List.empty,
            raw = Json.obj("unavailable" -> Json.fromString(reason))
        )

    def fromRawText(provider: String, jobId: String, deviceId: Option[String], raw: String): QuantumJobResult =
        parse(raw) match {
            case Right(json) => fromRawJson(provider, jobId, deviceId, json)
            case Left(_) =>
                QuantumJobResult(
                    provider = provider,
                    jobId = jobId,
                    deviceId = deviceId,
                    shots = None,
                    counts = Map.empty,
                    probabilities = Map.empty,
                    measurements = None,
                    resultTypes = List.empty,
                    raw = Json.fromString(raw)
                )
        }

    def fromRawJson(provider: String, jobId: String, deviceId: Option[String], raw: Json): QuantumJobResult =
        QuantumJobResult(
            provider = provider,
            jobId = jobId,
            deviceId = deviceId.orElse(findString(raw, Set("deviceArn", "backend", "device_id", "deviceId"))),
            shots = findInt(raw, Set("shots", "numShots", "num_shots", "numSuccessfulShots")),
            counts = findLongMap(raw, Set("counts", "measurement_counts", "measurementCounts")),
            probabilities = findDoubleMap(raw, Set("probabilities", "measurement_probabilities", "measurementProbabilities")),
            measurements = findMeasurements(raw, Set("measurements", "measurement_results", "measurementResults")),
            resultTypes = findResultTypes(raw),
            raw = raw
        )

    private def allFields(json: Json): List[(String, Json)] =
        json.asObject.toList.flatMap { obj =>
            obj.toList.flatMap { case (key, value) =>
                (key -> value) :: allFields(value)
            }
        } ++ json.asArray.toList.flatMap(_.toList.flatMap(allFields))

    private def findField(json: Json, names: Set[String]): Option[Json] =
        allFields(json).collectFirst { case (key, value) if names.contains(key) => value }

    private def findString(json: Json, names: Set[String]): Option[String] =
        findField(json, names).flatMap(_.asString)

    private def findInt(json: Json, names: Set[String]): Option[Int] =
        findField(json, names).flatMap { json =>
            json.asNumber.flatMap(_.toInt).orElse(json.asString.flatMap(_.toIntOption))
        }

    private def findLongMap(json: Json, names: Set[String]): Map[String, Long] =
        findField(json, names)
            .flatMap(_.asObject)
            .map(jsonObjectToLongMap)
            .getOrElse(Map.empty)

    private def findDoubleMap(json: Json, names: Set[String]): Map[String, Double] =
        findField(json, names)
            .flatMap(_.asObject)
            .map(jsonObjectToDoubleMap)
            .getOrElse(Map.empty)

    private def jsonObjectToLongMap(obj: JsonObject): Map[String, Long] =
        obj.toMap.flatMap { case (key, value) =>
            value.asNumber
                .flatMap(_.toLong)
                .orElse(value.asString.flatMap(_.toLongOption))
                .map(key -> _)
        }

    private def jsonObjectToDoubleMap(obj: JsonObject): Map[String, Double] =
        obj.toMap.flatMap { case (key, value) =>
            value.asNumber
                .map(_.toDouble)
                .orElse(value.asString.flatMap(_.toDoubleOption))
                .map(key -> _)
        }

    private def findMeasurements(json: Json, names: Set[String]): Option[List[List[Int]]] =
        findField(json, names).flatMap { value =>
            value.asArray.map { rows =>
                rows.toList.flatMap { row =>
                    row.asArray.map(_.toList.flatMap(v => v.asNumber.flatMap(_.toInt)))
                }
            }
        }

    private def findResultTypes(json: Json): List[QuantumResultValue] =
        findField(json, Set("resultTypes", "result_types")).flatMap(_.asArray).toList.flatten.toList.map { value =>
            val cursor = value.hcursor
            val typeJson =
                cursor.downField("type").focus
                    .orElse(cursor.downField("resultType").focus)
                    .getOrElse(Json.Null)

            val name =
                typeJson.asString
                    .orElse(typeJson.hcursor.downField("type").as[String].toOption)
                    .orElse(typeJson.hcursor.downField("name").as[String].toOption)
                    .getOrElse("unknown")

            val targets =
                typeJson.hcursor.downField("targets").as[List[Int]].toOption
                    .orElse(cursor.downField("targets").as[List[Int]].toOption)
                    .getOrElse(List.empty)

            val resultValue =
                cursor.downField("value").focus
                    .orElse(cursor.downField("result").focus)
                    .getOrElse(value)

            QuantumResultValue(name, targets, resultValue)
        }
}
