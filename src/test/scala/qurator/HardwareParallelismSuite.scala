package qurator.programs

import cats.effect.IO
import weaver.SimpleIOSuite
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.util.FidelityEstimator

object HardwareParallelismSuite extends SimpleIOSuite {
  private def approx(a: Double, b: Double) = expect(math.abs(a - b) < 1e-10)
  private def cal(model: ExecutionModel, t2: Double = 1.0): CanonicalCalibration =
    CanonicalCalibration(
      Map.empty, Map.empty, Some(0.0), Some(0.0), Map.empty, Some(1.0),
      Map.empty, Map.empty, Some(1.0), Some(t2), Map.empty, Map.empty,
      Some(0L), Some(10L), Some(40L), executionModel = model
    )
  private val zones = IonTrapParallelism(
    gateZones = 2, laserChannels = 2, zoneByQubit = Map(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1),
    concurrentGateError = Some(0.0)
  )
  private val atoms = NeutralAtomParallelism(
    positions = Map(0 -> AtomPosition(0, 0), 1 -> AtomPosition(1, 0),
      2 -> AtomPosition(10, 0), 3 -> AtomPosition(11, 0)),
    blockadeRadiusUm = Some(2.0), addressingExclusionRadiusUm = Some(0.5), maxParallelGates = 4,
    laserWaistUm = Some(1.0), crosstalkErrorAtZeroDistance = Some(0.0)
  )
  private def score(ops: List[Gate], calibration: CanonicalCalibration): FidelityEstimate =
    FidelityEstimator.score(Circuit(ops, 4), calibration)

  test("independent ion zones overlap, shared zones and laser channels serialize") {
    val ops = List(CX(0, 1), CX(2, 3))
    val concurrent = FidelityEstimator.estimateSchedule(ops, cal(ExecutionModel.TrappedIon(zones)))
    val sharedLaser = FidelityEstimator.estimateSchedule(ops, cal(ExecutionModel.TrappedIon(zones.copy(laserChannels = 1))))
    val sharedZone = FidelityEstimator.estimateSchedule(ops,
      cal(ExecutionModel.TrappedIon(zones.copy(zoneByQubit = (0 to 3).map(_ -> 0).toMap))))
    IO.pure(expect(concurrent.makespanNs == 40L) and expect(concurrent.peakGateParallelism == 2) and
      approx(concurrent.averageGateParallelism, 2.0) and expect(sharedLaser.makespanNs == 80L) and
      expect(sharedZone.makespanNs == 80L))
  }

  test("missing ion zone assignments cannot invent multi-zone concurrency") {
    val result = FidelityEstimator.estimateSchedule(List(CX(0, 1), CX(2, 3)),
      cal(ExecutionModel.TrappedIon(zones.copy(zoneByQubit = Map(0 -> 0)))))
    IO.pure(expect(result.makespanNs == 80L) and expect(result.warnings.exists(_.contains("Incomplete ion zone"))))
  }

  test("cross-zone ion round trips and cooling add exposure and transport loss") {
    val profile = zones.copy(splitNs = 2, shuttleNs = 5, mergeNs = 3, coolingNs = 7, transportErrorPerLeg = 0.01)
    val out = score(List(CX(0, 2)), cal(ExecutionModel.TrappedIon(profile)))
    val schedule = out.schedule.get
    IO.pure(expect(schedule.makespanNs == 67L) and expect(schedule.operations.head.gateStartNs == 27L) and
      approx(out.logPDecoh, -27e-9) and approx(out.logPTransport, 2 * math.log(0.99)) and
      approx(out.logPTotal, out.logPOps + out.logPDecoh + out.logPCrosstalk + out.logPTransport))
  }

  test("transport channels constrain otherwise separate zone pairs") {
    val p = zones.copy(gateZones = 4, laserChannels = 4,
      zoneByQubit = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3), shuttleNs = 10L)
    val ops = List(CX(0, 1), CX(2, 3))
    val one = FidelityEstimator.estimateSchedule(ops, cal(ExecutionModel.TrappedIon(p)))
    val two = FidelityEstimator.estimateSchedule(ops, cal(ExecutionModel.TrappedIon(p.copy(transportChannels = 2))))
    IO.pure(expect(one.makespanNs == 120L) and expect(two.makespanNs == 60L))
  }

  test("neutral-atom disjoint pairs overlap only outside blockade exclusion") {
    val ops = List(CZ(0, 1), CZ(2, 3))
    val far = FidelityEstimator.estimateSchedule(ops, cal(ExecutionModel.NeutralAtom(atoms)))
    val near = atoms.copy(positions = atoms.positions ++ Map(2 -> AtomPosition(2, 0), 3 -> AtomPosition(3, 0)))
    val blocked = FidelityEstimator.estimateSchedule(ops, cal(ExecutionModel.NeutralAtom(near)))
    IO.pure(expect(far.makespanNs == 40L) and expect(far.peakGateParallelism == 2) and
      expect(blocked.makespanNs == 80L) and expect(blocked.peakGateParallelism == 1))
  }

  test("local addressing exclusion also constrains single-qubit gates") {
    val profile = atoms.copy(addressingExclusionRadiusUm = Some(1.1))
    val result = FidelityEstimator.estimateSchedule(List(X(0), X(1)), cal(ExecutionModel.NeutralAtom(profile)))
    IO.pure(expect(result.makespanNs == 20L))
  }

  test("atom position uncertainty expands the exclusion region") {
    val p = atoms.copy(addressingExclusionRadiusUm = Some(0.8))
    val exact = FidelityEstimator.estimateSchedule(List(X(0), X(1)), cal(ExecutionModel.NeutralAtom(p)))
    val uncertain = FidelityEstimator.estimateSchedule(List(X(0), X(1)),
      cal(ExecutionModel.NeutralAtom(p.copy(positionUncertaintyUm = 0.15))))
    IO.pure(expect(exact.makespanNs == 10L) and expect(uncertain.makespanNs == 20L))
  }

  test("unknown atom coordinates and exclusion radii do not imply unlimited concurrency") {
    val p = atoms.copy(positions = Map.empty)
    val unknown = FidelityEstimator.estimateSchedule(List(CZ(0, 1), CZ(2, 3)), cal(ExecutionModel.NeutralAtom(p)))
    val noRadius = FidelityEstimator.estimateSchedule(List(CZ(0, 1), CZ(2, 3)),
      cal(ExecutionModel.NeutralAtom(atoms.copy(blockadeRadiusUm = None))))
    IO.pure(expect(unknown.makespanNs == 80L) and expect(noRadius.makespanNs == 80L) and expect(unknown.warnings.nonEmpty))
  }

  test("neutral-atom addressing capacity limits spatially separated gates") {
    val result = FidelityEstimator.estimateSchedule(List(X(0), X(2)),
      cal(ExecutionModel.NeutralAtom(atoms.copy(maxParallelGates = 1))))
    IO.pure(expect(result.makespanNs == 20L))
  }

  test("out-of-range neutral interactions reserve exclusive movement time and survival") {
    val p = atoms.copy(roundTripMoveNs = Some(100L), moveSurvival = 0.98)
    val out = score(List(CZ(0, 2), X(3)), cal(ExecutionModel.NeutralAtom(p)))
    IO.pure(expect(out.schedule.get.makespanNs == 150L) and
      expect(out.schedule.get.operations.head.gateStartNs == 100L) and
      approx(out.logPTransport, math.log(0.98)) and approx(out.logPDecoh, -180e-9))
  }

  test("measurement barriers synchronize readout without serializing readout channels") {
    val base = cal(ExecutionModel.TrappedIon(zones)).copy(durMeasNs = Some(100L))
    val out = FidelityEstimator.estimateSchedule(List(X(0), CX(2, 3), Measure(0), Measure(2)), base)
    IO.pure(expect(out.operations(2).startNs == 40L) and expect(out.operations(3).startNs == 40L) and
      expect(out.makespanNs == 140L))
  }

  test("neutral-atom reuse after measurement charges reload time and survival") {
    val p = atoms.copy(reloadNs = Some(50L), reloadSurvival = 0.9)
    val out = score(List(X(0), Measure(0), X(0)), cal(ExecutionModel.NeutralAtom(p)).copy(durMeasNs = Some(20L)))
    IO.pure(expect(out.schedule.get.makespanNs == 90L) and approx(out.logPTransport, math.log(0.9)))
  }

  test("CWCS trades optical crosstalk against coherence rather than always maximizing concurrency") {
    val p = atoms.copy(laserWaistUm = Some(20.0), crosstalkErrorAtZeroDistance = Some(0.2))
    val base = cal(ExecutionModel.NeutralAtom(p)).copy(dur1qAvgNs = Some(1000000L))
    val ops = List(X(0), X(2), Measure(0), Measure(2))
    val shortCoherence = score(ops, base.copy(t2Avg = Some(0.001)))
    val longCoherence = score(ops, base.copy(t2Avg = Some(1.0)))
    val expectedOverlapLog = math.log1p(-0.2 * math.exp(-0.5))
    IO.pure(expect(shortCoherence.schedule.get.peakGateParallelism == 2) and
      approx(shortCoherence.logPCrosstalk, expectedOverlapLog) and
      approx(shortCoherence.logPTotal, expectedOverlapLog) and
      expect(longCoherence.schedule.get.peakGateParallelism == 1) and
      approx(longCoherence.logPCrosstalk, 0.0) and approx(longCoherence.logPTotal, -0.001))
  }

  test("CWCS changes concurrency with T1 while T2 and gate errors stay fixed") {
    val p = atoms.copy(laserWaistUm = Some(20.0), crosstalkErrorAtZeroDistance = Some(0.15))
    val base = cal(ExecutionModel.NeutralAtom(p), t2 = 0.01).copy(dur1qAvgNs = Some(1000000L))
    val ops = List(X(0), X(2), Measure(0), Measure(2))
    val shortT1 = score(ops, base.copy(t1Avg = Some(0.005)))
    val longT1 = score(ops, base.copy(t1Avg = Some(1.0)))
    IO.pure(expect(shortT1.schedule.get.peakGateParallelism == 2) and
      expect(longT1.schedule.get.peakGateParallelism == 1) and
      expect(shortT1.decoherence.get.isComplete) and expect(longT1.decoherence.get.isComplete) and
      approx(longT1.logPDecoh, -0.002 * (1.0 / 6.0 + 1.0 / 0.03)))
  }

  test("ion residual crosstalk is charged once per overlapping pair") {
    val p = zones.copy(concurrentGateError = Some(0.01))
    val out = score(List(CX(0, 1), CX(2, 3)), cal(ExecutionModel.TrappedIon(p), t2 = 1e-6))
    IO.pure(expect(out.schedule.get.peakGateParallelism == 2) and approx(out.logPCrosstalk, math.log(0.99)))
  }

  test("partial pulse overlap weights crosstalk by actual overlapping time") {
    val p = zones.copy(concurrentGateError = Some(0.001))
    // First pulse on zone 0 lasts 40ns; zone 1 starts its 40ns pulse after 10ns.
    val out = score(List(CX(0, 1), X(2), CX(2, 3)), cal(ExecutionModel.TrappedIon(p), t2 = 1e-6))
    IO.pure(expect(out.schedule.get.makespanNs == 50L) and
      approx(out.logPCrosstalk, 1.75 * math.log(0.999)))
  }

  test("capacity checks permit temporal holes and preserve every qubit dependency") {
    val p = zones.copy(gateZones = 3, zoneByQubit = Map(0 -> 0, 1 -> 1, 2 -> 2))
    val c = cal(ExecutionModel.TrappedIon(p)).copy(dur1qNs = Map("H" -> 100L))
    val s = FidelityEstimator.estimateSchedule(List(H(0), X(0), X(1), X(2)), c)
    // q0's second gate reserves a future interval; it must not occupy a laser now.
    IO.pure(expect(s.operations(1).startNs == 100L) and expect(s.operations(2).startNs == 0L) and
      expect(s.operations(3).startNs == 10L) and expect(s.makespanNs == 110L))
  }

  test("zero-duration virtual gates consume no ion control capacity") {
    val c = cal(ExecutionModel.TrappedIon(zones.copy(laserChannels = 1))).copy(dur1qNs = Map("RZ" -> 0L))
    val s = FidelityEstimator.estimateSchedule(List(X(0), RZ("pi", 2), X(2)), c)
    IO.pure(expect(s.operations(1).startNs == 0L) and expect(s.operations(1).endNs == 0L) and
      expect(s.operations(2).startNs == 10L) and expect(s.peakGateParallelism == 1))
  }

  test("native SX and RY pulses and RX-specific durations enter timing and gate fidelity") {
    val c = cal(ExecutionModel.TrappedIon(zones)).copy(
      dur1qNs = Map("RX" -> 7L, "SX" -> 3L, "RY" -> 5L), eps1qAvg = Some(0.01))
    val out = score(List(SX(0), RY("pi", 0), RX("pi", 0)), c)
    IO.pure(expect(out.schedule.get.makespanNs == 15L) and approx(out.logPOps, 3 * math.log(0.99)))
  }

  test("raw ion profiles and explicit neutral-atom gate calibration survive normalization") {
    val rawIon = IonQCalibration(1, 1, 99, 98, 99, 1e-6, 2e-6, 1e-6, parallelism = Some(zones))
    val rawAqt = AQTCalibration(1, 1, 99, 1e-6, 1e-6, 99, 2e-6, 98, parallelism = Some(zones))
    val rawAtom = NeutralAtomCalibration(0.01, 0.02, 0.99, 10L, 40L, 100L, Some(1.0), Some(1.0), atoms)
    val atom = FidelityEstimator.normalizeCalibration(rawAtom)
    IO.pure(expect(FidelityEstimator.normalizeCalibration(rawIon).executionModel == ExecutionModel.TrappedIon(zones)) and
      expect(FidelityEstimator.normalizeCalibration(rawAqt).executionModel == ExecutionModel.TrappedIon(zones)) and
      expect(atom.executionModel == ExecutionModel.NeutralAtom(atoms)) and expect(atom.dur2qAvgNs.contains(40L)))
  }

  test("analog QuEra metadata is explicitly incomplete for gate-model scoring") {
    val raw = QuEraCalibration(0.01, 0.01, None, None, None, Some(1.0), Some(1.0), None)
    val out = score(List(CZ(0, 1)), FidelityEstimator.normalizeCalibration(raw))
    IO.pure(expect(out.schedule.get.warnings.exists(_.contains("AHS"))) and
      expect(out.schedule.get.warnings.exists(_.contains("Missing duration"))))
  }

  test("atom initialization survival enters fidelity once per used physical atom") {
    val c = cal(ExecutionModel.NeutralAtom(atoms)).copy(initSurvivalPerQubit = Some(0.9))
    val out = score(List(X(0), X(0), X(2)), c)
    IO.pure(approx(out.logPInitialization, 2 * math.log(0.9)) and
      approx(out.logPTotal, out.logPOps + out.logPDecoh + out.logPInitialization))
  }

  test("empty circuits have finite neutral-atom fidelity and zero parallelism") {
    val out = score(Nil, cal(ExecutionModel.NeutralAtom(atoms)).copy(initSurvivalPerQubit = Some(0.0)))
    IO.pure(approx(out.pTotal, 1.0) and expect(out.schedule.get.makespanNs == 0L) and
      expect(out.schedule.get.peakGateParallelism == 0) and approx(out.schedule.get.averageGateParallelism, 0.0))
  }

  test("neutral-atom topology preserves physical IDs and distinguishes static from movable arrays") {
    val p = atoms.copy(positions = Map(7 -> AtomPosition(0, 0), 19 -> AtomPosition(1, 0), 31 -> AtomPosition(20, 0)))
    val raw = NeutralAtomCalibration(0.01, 0.02, 0.99, 10L, 40L, 100L, Some(1.0), Some(1.0), p)
    val fixed = topologyOf(raw).get
    val movable = topologyOf(raw.copy(parallelism = p.copy(roundTripMoveNs = Some(100L)))).get
    IO.pure(expect(fixed.qubits == List(7, 19, 31)) and expect(fixed.edges == List(7 -> 19)) and
      expect(movable.edges.size == 3))
  }

  test("generated ion schedules respect dependencies and all resource capacities") {
    val rng = new scala.util.Random(29L)
    val p = zones.copy(gateZones = 4, laserChannels = 2, transportChannels = 1,
      zoneByQubit = (0 until 8).map(q => q -> (q / 2)).toMap,
      shuttleNs = 5L, coolingNs = 3L, concurrentGateError = Some(0.001))
    val results = (0 until 30).map { _ =>
      val ops = (0 until 25).map { _ =>
        val a = rng.nextInt(8)
        if (rng.nextBoolean()) X(a)
        else CX(a, (a + 1 + rng.nextInt(7)) % 8)
      }.toList
      val schedule = FidelityEstimator.estimateSchedule(ops, cal(ExecutionModel.TrappedIon(p), t2 = 1e-5))
      val timed = schedule.operations
      val dependencies = (0 until 8).forall { q =>
        timed.filter(_.qubits.contains(q)).sliding(2).forall {
          case Vector(a, b) => a.endNs <= b.startNs
          case _ => true
        }
      }
      val capacities = timed.flatMap(g => Vector(g.startNs, g.endNs)).distinct.forall { time =>
        val active = timed.filter(g => g.startNs <= time && time < g.endNs)
        active.size <= 2 &&
          active.count(_.resources.contains("transport")) <= 1 &&
          (0 until 4).forall(z => active.count(_.resources.contains(s"zone:$z")) <= 1)
      }
      dependencies && capacities && schedule.logPCrosstalk <= 0.0 && schedule.makespanNs > 0L
    }
    IO.pure(expect(results.forall(identity)))
  }

  test("invalid resource metadata fails explicitly rather than producing NaN schedules") {
    val failures = List(
      scala.util.Try(zones.copy(laserChannels = 0)).isFailure,
      scala.util.Try(zones.copy(concurrentGateError = Some(Double.NaN))).isFailure,
      scala.util.Try(atoms.copy(blockadeRadiusUm = Some(-1.0))).isFailure,
      scala.util.Try(atoms.copy(positionUncertaintyUm = Double.PositiveInfinity)).isFailure,
      scala.util.Try(atoms.copy(moveSurvival = 1.1)).isFailure
    )
    IO.pure(expect(failures.forall(identity)))
  }
}
