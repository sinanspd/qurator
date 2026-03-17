package qurator.domain

import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import eu.timepit.refined.types.string.NonEmptyString

object CutQC {

    case class CutQCConfig(
        baseUri: NonEmptyString
    )

    @derive(decoder, encoder, eqv, show)
    case class CutRequest(
        circuit: String,
        max_cuts: Int,
        max_subcircuits: Int,
        max_subcircuit_width: Int,
        subcircuit_size_imbalance: Double
    )

    @derive(decoder, encoder, eqv, show)
    case class CutResponse(
        subcircuits: List[String]
    )

    @derive(decoder, encoder, eqv, show)
    case class ErrorResponse(
        error: Error
    )

    @derive(decoder, encoder, eqv, show)
    case class Error(
        code: Int,
        message: String,
        explain: String
    )

}
