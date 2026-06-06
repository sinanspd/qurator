package qurator

import cats.effect.IO
import io.circe.Json
import qurator.domain.QuantumJobResult
import weaver.SimpleIOSuite

object QuantumJobResultSuite extends SimpleIOSuite {

    test("normalizes provider measurement payloads") {
        val raw =
            """
              |{
              |  "taskMetadata": { "shots": 100 },
              |  "measurementCounts": { "00": 60, "11": 40 },
              |  "measurementProbabilities": { "00": 0.6, "11": 0.4 },
              |  "measurements": [[0, 0], [1, 1]],
              |  "resultTypes": [
              |    {
              |      "type": { "type": "expectation", "targets": [0] },
              |      "value": 0.42
              |    }
              |  ]
              |}
              |""".stripMargin

        val result = QuantumJobResult.fromRawText("Braket", "job-1", Some("device-1"), raw)

        IO.pure(expect(result.shots.contains(100)) and
        expect(result.counts == Map("00" -> 60L, "11" -> 40L)) and
        expect(result.probabilities == Map("00" -> 0.6, "11" -> 0.4)) and
        expect(result.measurements.contains(List(List(0, 0), List(1, 1)))) and
        expect(result.resultTypes.map(_.name) == List("expectation")) and
        expect(result.resultTypes.headOption.map(_.value).contains(Json.fromDoubleOrNull(0.42))))
    }

    test("preserves non-json provider payloads") {
        val result = QuantumJobResult.fromRawText("IBM", "job-2", None, "raw provider result")

        IO.pure(expect(result.raw == Json.fromString("raw provider result")) and
        expect(result.counts.isEmpty) and
        expect(result.summary == "quantum result available"))
    }
}
