package qurator.domain

import io.circe.generic.auto.*
import cats.derived.*
import cats.Show
import cats.Eq
import eu.timepit.refined.types.string.NonEmptyString

object CutQC {

    case class CutQCConfig(
        baseUri: NonEmptyString
    )
  
    case class CutRequest(
        circuit: String,
        max_cuts: Int,
        max_subcircuits: Int,
        max_subcircuit_width: Int,
        subcircuit_size_imbalance: Double
    ) derives Eq, Show
  
    case class CutResponse(
        subcircuits: List[String]
    ) derives Eq, Show
  
    case class ErrorResponse(
        error: Error
    ) derives Eq, Show
  
    case class Error(
        code: Int,
        message: String,
        explain: String
    ) derives Eq, Show

}
