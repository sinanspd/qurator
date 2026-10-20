package qurator.testbed.fidelity

import qurator.domain.calibration.CanonicalCalibration
import qurator.domain.circuit.Circuit
import qurator.util.FidelityEstimator

trait BenchmarkFidelityEvaluator {
  def id: String
  def evaluate(compiled: Circuit, calibration: CanonicalCalibration): FidelityEvaluation
}

final case class FidelityEvaluation(
    evaluator: String,
    metric: String,
    score: Double,
    logScore: Double,
    samples: Option[Int] = None,
    seed: Option[Long] = None,
    confidence95: Option[(Double, Double)] = None,
    approximatedGates: Int = 0,
    warnings: Vector[String] = Vector.empty
)

object BenchmarkFidelityEvaluator {
  // Controls evaluation for every policy and the HighestFidelity baseline's device selection.
  val backend: String = "pauli"
  val pauliSettings: PauliTraceEvaluator.Settings = PauliTraceEvaluator.Settings(
    samples = 2048,
    seed = 42L,
    strictCalibration = false,
    errorConvention = PauliTraceEvaluator.AverageInfidelity
  )

  /** The original calculation, without changing its semantics or numerical results. */
  case object Legacy extends BenchmarkFidelityEvaluator {
    val id = "legacy"
    def evaluate(compiled: Circuit, calibration: CanonicalCalibration): FidelityEvaluation = {
      val estimate = FidelityEstimator.score(compiled, calibration)
      FidelityEvaluation(id, "estimated_success_probability", estimate.pTotal, estimate.logPTotal,
        warnings = calibration.warnings)
    }
  }

  /** Build once at the benchmark boundary, then inject the same evaluator into every policy. */
  def configured: BenchmarkFidelityEvaluator =
    backend.trim.toLowerCase match {
      case "legacy" => Legacy
      case "pauli" => new PauliTraceEvaluator(pauliSettings)
      case other => throw new IllegalArgumentException(
        s"Unknown benchmark fidelity evaluator '$other'; choose legacy or pauli")
    }
}
