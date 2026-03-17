package qurator.programs

import cats.effect.IO
import weaver.SimpleIOSuite
import qurator.domain.circuit._
import qurator.domain.calibration._
import qurator.util.FidelityEstimator

object FidelityEstimatorSuite extends SimpleIOSuite {

  private def approx(actual: Double, expected: Double, tol: Double = 1e-12) =
    expect(math.abs(actual - expected) <= tol)

  private def approxOpt(actual: Option[Double], expected: Option[Double], tol: Double = 1e-12) =
    (actual, expected) match {
      case (Some(a), Some(e)) => approx(a, e, tol)
      case (None, None)       => success
      case _                  => failure(s"expected $expected, got $actual")
    }

  private def mkCircuit(ops: List[Gate]): Circuit =
    Circuit(
      remainingGates = ops,
      qubits = 10,
      name = "test circuit"
    )

  private def mkCanonical(
    eps1qAvg: Option[Double] = Some(0.01),
    eps2qAvg: Option[Double] = Some(0.02),
    readoutFidelityAvg: Option[Double] = Some(0.95),
    t1Avg: Option[Double] = None,
    t2Avg: Option[Double] = None,
    durMeasNs: Option[Long] = Some(100L),
    dur1qAvgNs: Option[Long] = Some(10L),
    dur2qAvgNs: Option[Long] = Some(40L)
  ): CanonicalCalibration =
    CanonicalCalibration(
      eps1q = Map.empty,
      eps2q = Map.empty,
      eps1qAvg = eps1qAvg,
      eps2qAvg = eps2qAvg,
      readoutFidelity = Map.empty,
      readoutFidelityAvg = readoutFidelityAvg,
      t1 = Map.empty,
      t2 = Map.empty,
      t1Avg = t1Avg,
      t2Avg = t2Avg,
      dur1qNs = Map.empty,
      dur2qNs = Map.empty,
      durMeasNs = durMeasNs,
      dur1qAvgNs = dur1qAvgNs,
      dur2qAvgNs = dur2qAvgNs,
      initSurvivalPerQubit = None
    )

  private def mkIonQCalibration(
    avg1qFidelityPct: Double,
    avg2qFidelityPct: Double,
    avgReadoutFidelity: Double,
    t1Seconds: Double,
    t2Seconds: Double,
    oneQGateDurationSec: Double,
    twoQGateDurationSec: Double,
    readoutDurationSec: Double
  ): IonQCalibration =
    IonQCalibration(
      avg1qFidelityPct = avg1qFidelityPct,
      avg2qFidelityPct = avg2qFidelityPct,
      avgReadoutFidelity = avgReadoutFidelity,
      t1Seconds = t1Seconds,
      t2Seconds = t2Seconds,
      oneQGateDurationSec = oneQGateDurationSec,
      twoQGateDurationSec = twoQGateDurationSec,
      readoutDurationSec = readoutDurationSec
    )

  private def mkIQMCalibration(
    t1: Double,
    t2: Double, 
    q1fidelity: Double,
    q2fidelity: Double, 
    readoutFidelity: Double
  ): IQMCalibration =
    IQMCalibration(
      t1: Double,
      t2: Double, 
      q1fidelity: Double,
      q2fidelity: Double, 
      readoutFidelity: Double
    )

  private def mkQuEraCalibration(
    typicalDetectionFalsePositive: Double,
    typicalDetectionFalseNegative: Double,
    typicalVacancyError: Option[Double],
    typicalFillingError: Option[Double],
    typicalAtomLossProbability: Option[Double],
    t1SingleSec: Option[Double],
    t2EchoSingleSec: Option[Double],
    t2SingleSec: Option[Double]
  ): QuEraCalibration =
    QuEraCalibration(
      typicalDetectionFalsePositive = typicalDetectionFalsePositive,
      typicalDetectionFalseNegative = typicalDetectionFalseNegative,
      typicalVacancyError = typicalVacancyError,
      typicalFillingError = typicalFillingError,
      typicalAtomLossProbability = typicalAtomLossProbability,
      t1SingleSec = t1SingleSec,
      t2EchoSingleSec = t2EchoSingleSec,
      t2SingleSec = t2SingleSec
    )

  private def mkRigettiCalibration(
    avg1qFidelityPct: Double,
    readoutFidelityPct: Double,
    swapFidelityPct: Double,
    t1Seconds: Double,
    t2Seconds: Double,
    swapGateDurationNs: Long,
    readoutDurationNs: Long,
    oneQGateDurationNs: Long,
    twoQGateDurationNs: Long
  ): RigettiCalibration =
    RigettiCalibration(
      avg1qFidelityPct = avg1qFidelityPct,
      readoutFidelityPct = readoutFidelityPct,
      swapFidelityPct = swapFidelityPct,
      t1Seconds = t1Seconds,
      t2Seconds = t2Seconds,
      swapGateDurationNs = swapGateDurationNs,
      readoutDurationNs = readoutDurationNs,
      oneQGateDurationNs = oneQGateDurationNs,
      twoQGateDurationNs = twoQGateDurationNs
    )

  private def mkAQTCalibration(
    oneQGateFidelity: Double,
    twoQGateFidelity: Double,
    readoutFidelity: Double,
    t1Seconds: Double,
    t2Seconds: Double,
    readoutDurationSec: Double,
    oneQGateDurationSec: Double,
    twoQGateDurationSec: Double
  ): AQTCalibration =
    AQTCalibration(
      oneQGateFidelity = oneQGateFidelity,
      twoQGateFidelity = twoQGateFidelity,
      readoutFidelity = readoutFidelity,
      t1Seconds = t1Seconds,
      t2Seconds = t2Seconds,
      readoutDurationSec = readoutDurationSec,
      oneQGateDurationSec = oneQGateDurationSec,
      twoQGateDurationSec = twoQGateDurationSec
    )

  test("normalizeCalibration QuEra mirrors neutral-atom normalization logic") {
    val in = mkQuEraCalibration(
      typicalDetectionFalsePositive = 0.03,
      typicalDetectionFalseNegative = 0.05,
      typicalVacancyError = None,
      typicalFillingError = Some(0.15),
      typicalAtomLossProbability = Some(0.02),
      t1SingleSec = Some(9.0),
      t2EchoSingleSec = None,
      t2SingleSec = Some(4.0)
    )

    val out = FidelityEstimator.normalizeCalibration(in)

    IO.pure(
        approxOpt(out.readoutFidelityAvg, Some(0.96)) and
        approxOpt(out.initSurvivalPerQubit, Some(0.85 * 0.98)) and
        approxOpt(out.t1Avg, Some(9.0)) and
        approxOpt(out.t2Avg, Some(4.0)) and
        expect(out.durMeasNs.isEmpty)
    )
  }

  test("normalizeCalibration Rigetti converts percentages and keeps SWAP-specific duration") {
    val in = mkRigettiCalibration(
      avg1qFidelityPct = 99.0,
      readoutFidelityPct = 93.0,
      swapFidelityPct = 96.0,
      t1Seconds = 11.0,
      t2Seconds = 7.0,
      swapGateDurationNs = 222L,
      readoutDurationNs = 333L,
      oneQGateDurationNs = 444L,
      twoQGateDurationNs = 555L
    )

    val out = FidelityEstimator.normalizeCalibration(in)

    IO.pure(
        approxOpt(out.eps1qAvg, Some(0.01)) and
        approxOpt(out.eps2qAvg, Some(0.04)) and
        approxOpt(out.readoutFidelityAvg, Some(0.93)) and
        approxOpt(out.t1Avg, Some(11.0)) and
        approxOpt(out.t2Avg, Some(7.0)) and
        expect(out.dur2qNs.get("SWAP").contains(222L)) and
        expect(out.durMeasNs.contains(333L)) and
        expect(out.dur1qAvgNs.contains(444L)) and
        expect(out.dur2qAvgNs.contains(555L))
    )
  }

  test("scheduleEndTimesSec runs independent 1q gates in parallel") {
    val cal = mkCanonical(
      dur1qAvgNs = Some(10L),
      dur2qAvgNs = Some(40L),
      durMeasNs = Some(100L)
    )

    val actual = FidelityEstimator.scheduleEndTimesSec(
      List(X(0), H(1)),
      cal
    )

    IO.pure(
        approx(actual(0), 10.0 / 1e9) and 
        approx(actual(1), 10.0 / 1e9)
      )
  }

  test("scheduleEndTimesSec accumulates sequential work on the same qubit") {
    val cal = mkCanonical(
      dur1qAvgNs = Some(10L),
      durMeasNs = Some(100L)
    )

    val actual = FidelityEstimator.scheduleEndTimesSec(
      List(X(0), H(0), Measure(0)),
      cal
    )

    IO.pure(
        approx(actual(0), 120.0 / 1e9) and 
        expect(actual.keySet == Set(0))
    )
  }

  test("scheduleEndTimesSec makes a 2q gate wait for the slower qubit and synchronizes both") {
    val cal = mkCanonical(
      dur1qAvgNs = Some(10L),
      dur2qAvgNs = Some(40L)
    )

    val actual = FidelityEstimator.scheduleEndTimesSec(
      List(
        X(0),   // q0: 10ns
        X(0),   // q0: 20ns
        H(1),   // q1: 10ns
        CX(0, 1) // starts at 20ns, ends at 60ns on both
      ),
      cal
    )

    IO.pure(
        approx(actual(0), 60.0 / 1e9) and
        approx(actual(1), 60.0 / 1e9)
    )
  }

  test("scheduleEndTimesSec lets measurements extend only the measured qubit timeline") {
    val cal = mkCanonical(
      dur1qAvgNs = Some(10L),
      dur2qAvgNs = Some(40L),
      durMeasNs = Some(100L)
    )

    val actual = FidelityEstimator.scheduleEndTimesSec(
      List(
        X(0),      // q0: 10
        H(1),      // q1: 10
        Measure(0) // q0: 110, q1 stays 10
      ),
      cal
    )

    IO.pure(
        approx(actual(0), 110.0 / 1e9) and 
        approx(actual(1), 10.0 / 1e9)
    )
  }

  test("score multiplies gate and readout success probabilities when there is no decoherence") {
    val cal = mkCanonical(
      eps1qAvg = Some(0.10),          
      eps2qAvg = Some(0.20),          
      readoutFidelityAvg = Some(0.90), 
      t2Avg = None,
      dur1qAvgNs = Some(10L),
      dur2qAvgNs = Some(40L),
      durMeasNs = Some(100L)
    )

    val circuit = mkCircuit(
      List(
        X(0),
        CX(0, 1),
        Measure(0),
        Measure(1)
      )
    )

    val out = FidelityEstimator.score(circuit, cal)

    val expectedLogPOps =
      math.log(0.90) + // X
      math.log(0.80) + // CX
      math.log(0.90) + // Measure(0)
      math.log(0.90)   // Measure(1)

    val expectedP = 0.90 * 0.80 * 0.90 * 0.90

    IO.pure(
        approx(out.logPOps, expectedLogPOps) and
        approx(out.logPDecoh, 0.0) and 
        approx(out.logPTotal, expectedLogPOps) and
        approx(out.pTotal, expectedP) and
        approx(out.perQubitEndTimeSec(0), 150.0 / 1e9) and 
        approx(out.perQubitEndTimeSec(1), 150.0 / 1e9)
    )
  }

  test("score adds T2 decoherence from each qubit end time") {
    val cal = mkCanonical(
      eps1qAvg = Some(0.10),
      eps2qAvg = Some(0.20),
      readoutFidelityAvg = Some(0.90),
      t2Avg = Some(1.0), 
      dur1qAvgNs = Some(10L),
      dur2qAvgNs = Some(40L),
      durMeasNs = Some(100L)
    )

    val circuit = mkCircuit(
      List(
        X(0),
        CX(0, 1),
        Measure(0),
        Measure(1)
      )
    )

    val out = FidelityEstimator.score(circuit, cal)

    val expectedLogPOps =
      math.log(0.90) + math.log(0.80) + math.log(0.90) + math.log(0.90)

    val perQubitEnd = 150.0 / 1e9
    val expectedLogPDecoh = -(perQubitEnd / 1.0) - (perQubitEnd / 1.0)
    val expectedTotal = expectedLogPOps + expectedLogPDecoh
    val expectedP = math.exp(expectedTotal)

    IO.pure(
        approx(out.logPOps, expectedLogPOps) and
        approx(out.logPDecoh, expectedLogPDecoh) and
        approx(out.logPTotal, expectedTotal) and
        approx(out.pTotal, expectedP) and
        approx(out.perQubitEndTimeSec(0), perQubitEnd) and
        approx(out.perQubitEndTimeSec(1), perQubitEnd)
    )
  }

  test("score ignores non-positive T2 values in the decoherence term") {
    val cal = mkCanonical(
      eps1qAvg = Some(0.10),
      eps2qAvg = Some(0.20),
      readoutFidelityAvg = Some(0.90),
      t2Avg = Some(-1.0), 
      dur1qAvgNs = Some(10L),
      dur2qAvgNs = Some(40L),
      durMeasNs = Some(100L)
    )

    val circuit = mkCircuit(
      List(
        X(0),
        CX(0, 1),
        Measure(0),
        Measure(1)
      )
    )

    val out = FidelityEstimator.score(circuit, cal)

    IO.pure(
        approx(out.logPDecoh, 0.0) and 
        approx(out.logPTotal, out.logPOps) and
        approx(out.pTotal, math.exp(out.logPOps))
    )
  }
}