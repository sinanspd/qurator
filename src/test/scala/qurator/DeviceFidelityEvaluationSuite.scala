package qurator.testbed.fidelity

import cats.effect.IO
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.device.Device
import qurator.testbed.{BenchmarkDeviceRegistry, HaqaMapper}
import qurator.util.FidelityEstimator
import weaver.SimpleIOSuite

object DeviceFidelityEvaluationSuite extends SimpleIOSuite {
  private val device = Device("test", "sparse-line", qubits = 3,
    t1 = 0f, t2 = 0f, gateSet = List(H(0), CZ(0, 1)))
  private val raw = BenchmarkDeviceRegistry.defaultCalibrations("braket-iqm-garnet")
    .asInstanceOf[IQMCalibration].copy(
      topology = Some(CalibrationTopology(List(2, 4, 7), List(2 -> 4, 4 -> 7))))
  // No placement of a triangle on a line can avoid routing at least one interaction.
  private val input = Circuit(
    List(H(0), CZ(0, 1), CZ(1, 2), CZ(0, 2), Measure(0), Measure(1), Measure(2)),
    3, "triangle")
  private val evaluator = new PauliTraceEvaluator(PauliTraceEvaluator.Settings(samples = 256, seed = 42L))
  private val mustNotEvaluate = new BenchmarkFidelityEvaluator {
    val id = "must-not-evaluate"
    def evaluate(compiled: Circuit, calibration: CanonicalCalibration): FidelityEvaluation =
      throw new AssertionError("Evaluation ran before compilation validation")
  }

  test("offline evaluation scores the placed trace and charges for inserted routing gates") {
    for {
      result <- DeviceFidelityEvaluation.run(input, device, raw, evaluator)
      compiled = result.compiledCircuit
      topology = HaqaMapper.topologyFromCalibration(raw).get
      expected = evaluator.evaluate(compiled, FidelityEstimator.normalizeCalibration(raw))
      interactions = compiled.remainingGates.map(gateKey).filter(_._2.size == 2)
    } yield expect(result.evaluation == expected) and
      expect(compiled.remainingGates != input.remainingGates) and
      expect(interactions.size > 3) and
      expect(interactions.forall { case (kind, qs) =>
        kind == "CZ" && topology.hasEdge(qs(0), qs(1))
      }) and
      expect(compiled.remainingGates.flatMap(gateKey(_)._2).toSet == Set(2, 4, 7)) and
      expect(result.physicalQubitsUsed == 3) and
      expect(compiled.qubits == 8) and
      expect(result.compilationMillis >= 0 && result.evaluationMillis >= 0)
  }

  test("missing topology fails before evaluation instead of scoring a raw circuit") {
    DeviceFidelityEvaluation.run(input, device, raw.copy(topology = None), mustNotEvaluate).attempt.map { result =>
      expect(result.left.exists(_.getMessage.contains("no calibration topology")))
    }
  }

  test("cached routes remain valid when the same interaction is requested in reverse") {
    val bothDirections = Circuit(List(
      CX(0, 1), CX(1, 2), CX(0, 2), CX(1, 0), CX(2, 1), CX(2, 0)), 3, "reverse-routes")
    DeviceFidelityEvaluation.compile(bothDirections, device, raw).map { compiled =>
      val topology = HaqaMapper.topologyFromCalibration(raw).get
      val pairs = compiled.remainingGates.map(gateKey).filter(_._2.size == 2)
      expect(pairs.size > 6) and expect(pairs.forall { case (_, qs) => topology.hasEdge(qs(0), qs(1)) })
    }
  }

  test("an oversized circuit is rejected before the compiler's permissive fallback") {
    DeviceFidelityEvaluation.run(input.copy(qubits = 4), device, raw, mustNotEvaluate).attempt.map { result =>
      expect(result.left.exists(_.getMessage.contains("Circuit needs 4 qubits")))
    }
  }

  test("disconnected hardware cannot produce a score for an unroutable interaction") {
    val disconnected = raw.copy(topology = Some(CalibrationTopology(List(2, 4, 7), List(2 -> 4))))
    DeviceFidelityEvaluation.run(input, device, disconnected, mustNotEvaluate).attempt.map { result =>
      expect(result.left.exists(_.getMessage.contains("could not route")))
    }
  }

  test("out-of-register operands and unlowered multi-qubit gates are rejected") {
    for {
      badOperand <- DeviceFidelityEvaluation.run(
        input.copy(remainingGates = List(CZ(0, 3))), device, raw, mustNotEvaluate).attempt
      unlowered <- DeviceFidelityEvaluation.run(
        input.copy(remainingGates = List(CCX(0, 1, 2))), device, raw, mustNotEvaluate).attempt
    } yield expect(badOperand.left.exists(_.getMessage.contains("outside its declared register"))) and
      expect(unlowered.left.exists(_.getMessage.contains("multi-qubit operation")))
  }
}
