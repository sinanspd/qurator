package qurator.testbed

import cats.effect.{IO, Ref}
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import qurator.domain.{ID, QuantumJobResult}
import qurator.domain.Task._
import qurator.domain.calibration.CanonicalCalibration
import qurator.domain.circuit._
import qurator.testbed.fidelity._
import weaver.SimpleIOSuite

object BenchmarkFidelityIntegrationSuite extends SimpleIOSuite {
  implicit val logger: Logger[IO] = NoOpLogger[IO]
  private val circuit = Circuit(List(H(0), CX(0, 1), Measure(0), Measure(1)), 2, "evaluation-test")
  private val spec = QuantumTaskSpec(circuit, TaskQubits(2), TaskShots(1000), TaskDepth(3))

  private def environment = for {
    devices <- IO(BenchmarkDeviceRegistry.defaultDevices.filter(_.platform == "IBM").take(2))
    fake <- devices.traverse(d => BenchmarkFakeDevice.make(d, 0, 0).map(d.platformId -> _))
    records <- Ref.of[IO, Map[String, JobRecord]](Map.empty)
    registry = BenchmarkDeviceRegistry(devices.map(d => d.platformId -> d).toMap, fake.toMap,
      BenchmarkDeviceRegistry.defaultCalibrations, devices.map(_.platformId -> 0).toMap, records)
    dummies <- FakeBenchmarkClientsFromRegistry.make(registry)
    clients = BenchmarkHttpClients.make(registry, dummies)
  } yield (registry, clients, FakeCompiler[IO](Nil))

  private class Probe extends BenchmarkFidelityEvaluator {
    val id = "test-probe"
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)
    var received: Option[Circuit] = None
    def evaluate(compiled: Circuit, cal: CanonicalCalibration): FidelityEvaluation = {
      calls.incrementAndGet()
      received = Some(compiled)
      FidelityEvaluation(id, "test-score", 0.123, math.log(0.123))
    }
  }

  test("switching evaluation preserves LeastBusy's choice and predictions") {
    val probe = new Probe
    for {
      a <- environment
      b <- environment
      legacy <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.LeastBusy,
        List(spec), a._1, a._2, a._3)
      switched <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.LeastBusy,
        List(spec), b._1, b._2, b._3, probe)
    } yield expect(legacy.quantumMetrics.map(_.deviceId) == switched.quantumMetrics.map(_.deviceId)) and
      expect(legacy.meanPredictedSuccessProbability == switched.meanPredictedSuccessProbability) and
      expect(legacy.meanEvaluationScore.contains(legacy.meanPredictedSuccessProbability)) and
      expect(switched.meanEvaluationScore.contains(0.123)) and expect(probe.calls.get == 1) and
      expect(probe.received.contains(circuit))
  }

  test("HighestFidelity follows the active evaluator, reuses the winning score, and switches back to legacy") {
    for {
      a <- environment
      b <- environment
      c <- environment
      legacy <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.HighestFidelity,
        List(spec), a._1, a._2, a._3)
      legacyDevice = legacy.quantumMetrics.head.deviceId
      compiler = FakeCompiler[IO](Nil, (device, input) => input.copy(name = device.platformId))
      calls = new java.util.concurrent.atomic.AtomicInteger(0)
      evaluator = new BenchmarkFidelityEvaluator {
        val id = "opposite-ranking"
        def evaluate(compiled: Circuit, cal: CanonicalCalibration): FidelityEvaluation = {
          calls.incrementAndGet()
          val score = if (compiled.name == legacyDevice) 0.1 else 0.9
          FidelityEvaluation(id, "test-score", score, math.log(score))
        }
      }
      switched <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.HighestFidelity,
        List(spec), b._1, b._2, compiler, evaluator)
      restored <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.HighestFidelity,
        List(spec), c._1, c._2, c._3, BenchmarkFidelityEvaluator.Legacy)
      selected = switched.quantumMetrics.head
      expectedPrediction <- qurator.programs.Scheduler.estimateFidelity[IO](
        b._1.device(selected.deviceId), circuit, b._2, compiler)
    } yield expect(selected.deviceId != legacyDevice) and
      expect(switched.meanEvaluationScore.contains(0.9)) and
      expect(calls.get == b._1.devices.size) and
      expect(selected.predictedSuccessProbability == expectedPrediction.pTotal) and
      expect(restored.quantumMetrics.head.deviceId == legacyDevice) and
      expect(restored.meanPredictedSuccessProbability == legacy.meanPredictedSuccessProbability) and
      expect(restored.meanEvaluationScore == legacy.meanEvaluationScore)
  }

  test("scheduler compiles the submitted merged circuit and evaluates it once per provider job") {
    val probe = new Probe
    val executed = circuit.copy(name = "actually-executed", remainingGates = X(0) :: circuit.remainingGates)
    val compiled = Circuit(List(X(2), H(2), CX(2, 5), Measure(2), Measure(5)), 6, "mapped-merged")
    val compilations = new java.util.concurrent.atomic.AtomicInteger(0)
    val compiler = FakeCompiler[IO](Nil, (_, input) => {
      require(input == executed)
      compilations.incrementAndGet()
      compiled
    })
    for {
      env <- environment
      device = env._1.devicesById.keys.toList.sorted.head
      _ <- env._1.recordProviderSubmission("job-merged", device)
      first <- ID.make[IO, TaskId]
      second <- ID.make[IO, TaskId]
      completions = List(first, second).map { taskId =>
        QuantumJobResult.unavailable("IBM", "job-merged", Some(device), "test")
          .copy(taskId = Some(taskId), executedCircuit = Some(executed))
      }
      metrics <- SchedulerBenchmarkRunner.quantumMetricsForAssignments(completions, env._1, env._2, compiler, probe)
    } yield expect(metrics.size == 2) and expect(probe.calls.get == 1) and
      expect(compilations.get == 1) and expect(probe.received.contains(compiled)) and
      expect(metrics.forall(_.evaluation.exists(_.score == 0.123)))
  }

  test("scheduler and baseline Pauli scores agree for the same circuit and selected device") {
    val evaluator = new PauliTraceEvaluator(PauliTraceEvaluator.Settings(samples = 256, seed = 42L))
    // Force native-gate lowering even when the compiler keeps the original qubit placement.
    val input = circuit.copy(remainingGates = Swap(0, 1) :: Swap(0, 1) :: circuit.remainingGates)
    for {
      env <- environment
      compiler = FakeCompiler.topologyAware[IO](env._1)
      baseline <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.HighestFidelity,
        List(spec.copy(circuit = input)), env._1, env._2, compiler, evaluator)
      selected = baseline.quantumMetrics.head
      candidateScores <- env._1.devices.traverse { device =>
        compiler.compileCircuitFor(device, input).map { candidate =>
          evaluator.evaluate(candidate, qurator.util.FidelityEstimator.normalizeCalibration(env._1.calibration(device.platformId))).score
        }
      }
      compiled <- compiler.compileCircuitFor(env._1.device(selected.deviceId), input)
      _ <- env._1.recordProviderSubmission("job-same-device", selected.deviceId)
      taskId <- ID.make[IO, TaskId]
      completion = QuantumJobResult.unavailable("IBM", "job-same-device", Some(selected.deviceId), "test")
        .copy(taskId = Some(taskId), executedCircuit = Some(input))
      metrics <- SchedulerBenchmarkRunner.quantumMetricsForAssignments(
        List(completion), env._1, env._2, compiler, evaluator)
      alreadyCompiledMetrics <- SchedulerBenchmarkRunner.quantumMetricsForAssignments(
        List(completion.copy(executedCircuit = Some(compiled))), env._1, env._2, compiler, evaluator)
      offline <- DeviceFidelityEvaluation.run(input, env._1.device(selected.deviceId),
        env._1.calibration(selected.deviceId), evaluator)
    } yield expect(compiled.remainingGates != input.remainingGates) and
      expect(selected.evaluation.exists(_.score == candidateScores.max)) and
      expect(metrics.head.evaluation == selected.evaluation) and
      expect(metrics.head.predictedSuccessProbability == selected.predictedSuccessProbability) and
      expect(alreadyCompiledMetrics.head.evaluation == selected.evaluation) and
      expect(offline.compiledCircuit == compiled) and
      expect(selected.evaluation.contains(offline.evaluation))
  }

  test("device-fit CSV keeps predictions separate and retains evaluation failures and uncertainty") {
    val score = FidelityEvaluation("pauli", "pauli_identity_survival_proxy", 0.4, math.log(0.4),
      samples = Some(256), seed = Some(42L), confidence95 = Some(0.34 -> 0.46),
      approximatedGates = 7, warnings = Vector("missing data"))
    val row = DeviceFitBenchmark.IterationResult(1, 2, 4, 2, Some(0.5), Some("a"), Some(0.7), Some("b"),
      Vector("a" -> Right(score), "b" -> Left("unsupported gate")))
    val legacy = DeviceFitBenchmark.Report(0, Vector.empty, Vector.empty, Vector.empty, Vector(row))
    val extended = legacy.copy(evaluationBackend = "pauli")
    IO.pure(expect(legacy.csvLines.head == DeviceFitBenchmark.csvHeader) and
      expect(extended.csvLines.head.contains("min_evaluation_score")) and
      expect(extended.csvLines(1).contains("unsupported gate")) and
      expect(extended.csvLines(1).contains("confidence95")) and
      expect(extended.csvLines(1).startsWith(row.csvLine + ",pauli,1,")))
  }
}
