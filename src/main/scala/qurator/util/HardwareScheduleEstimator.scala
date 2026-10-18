package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import scala.collection.mutable

/** Coherence-weighted conflict scheduling (CWCS).
  *
  * Reconstructs feasible schedules from dependencies, resource capacities and spatial
  * exclusion. Compares earliest feasible, locally exposure-aware, and serialized
  * schedules using the complete circuit's log decoherence + overlap loss. This is
  * a ranking heuristic, not a pulse compiler or a hardware-validated noise model.
  */
object HardwareScheduleEstimator {
  private case class Operation(
      index: Int,
      qubits: Vector[Int],
      durationNs: Long,
      setupNs: Long,
      measurement: Boolean,
      resources: Map[String, Int],
      exclusive: Boolean,
      logSurvival: Double
  )

  private case class Reservation(op: Operation, timed: TimedGate)

  def estimate(ops: List[Gate], cal: CanonicalCalibration): HardwareSchedule = {
    val warnings = mutable.LinkedHashSet.empty[String]
    warnings ++= cal.warnings
    val usedQubits = ops.iterator.flatMap(gateKey(_)._2).toSet
    val model = cal.executionModel match {
      case ExecutionModel.TrappedIon(p) if p.gateZones > 1 && !usedQubits.subsetOf(p.zoneByQubit.keySet) =>
        warnings += "Incomplete ion zone assignments: assuming one shared gate zone and control channel."
        ExecutionModel.TrappedIon(p.copy(gateZones = 1, laserChannels = 1, zoneByQubit = Map.empty))
      case other => other
    }
    val blocksMeasurement = model match {
      case ExecutionModel.TrappedIon(p) => p.measurementBlocksGates
      case ExecutionModel.NeutralAtom(p) => p.measurementBlocksGates
      case _ => false
    }
    model match {
      case ExecutionModel.TrappedIon(p) if p.concurrentGateError.isEmpty =>
        warnings += "Ion concurrent-gate error is uncalibrated; no residual crosstalk penalty is assumed."
      case ExecutionModel.NeutralAtom(p) =>
        val interactionRadius = if (p.requireInteractionExclusionForAllGates) p.interactionExclusionRadiusUm
          else p.interactionExclusionRadiusUm.orElse(p.blockadeRadiusUm)
        if (!usedQubits.subsetOf(p.positions.keySet) || interactionRadius.isEmpty || p.addressingExclusionRadiusUm.isEmpty)
          warnings += "Incomplete atom geometry/exclusion radii: independent gates are serialized when spatial safety is unknown."
        if (p.requireInteractionExclusionForAllGates &&
            ops.exists(op => gateKey(op)._2.size == 2) && (p.blockadeRadiusUm.isEmpty || !usedQubits.subsetOf(p.positions.keySet)))
          warnings += "Two-atom interaction reach is unverified: missing blockade radius or atom placement."
        if (p.laserWaistUm.isEmpty || p.crosstalkErrorAtZeroDistance.isEmpty)
          warnings += "Neutral-atom optical crosstalk is uncalibrated; only hard exclusion constraints are applied."
      case _ => ()
    }

    val hasHardwareResources = model match {
      case _: ExecutionModel.TrappedIon | _: ExecutionModel.NeutralAtom => true
      case _ => false
    }
    val hasSoftNoise = model match {
      case ExecutionModel.TrappedIon(p) => p.concurrentGateError.exists(_ > 0.0)
      case ExecutionModel.NeutralAtom(p) => p.crosstalkErrorAtZeroDistance.exists(_ > 0.0) && p.laserWaistUm.nonEmpty
      case _ => false
    }
    // A round-trip approximation keeps physical IDs/home positions stable between gates.
    val measured = mutable.Set.empty[Int]
    val operations = ops.zipWithIndex.map { case (gate, index) =>
      val (name, rawQubits) = gateKey(gate)
      val qs = rawQubits.distinct
      val measurement = gate.isInstanceOf[Measure]
      if (cal.durationNsOptionFor(gate).isEmpty && qs.nonEmpty)
        warnings += s"Missing duration for $name; timing and fidelity are incomplete."
      if (qs.size > 2 && name != "BARRIER")
        warnings += s"$name must be lowered to one-/two-qubit gates for calibrated fidelity."
      val duration = cal.durationNsFor(gate)
      val pulse = !measurement && duration > 0L && qs.nonEmpty
      var setup = 0L
      var resources = Map.empty[String, Int]
      var exclusive = false
      var logSurvival = 0.0
      if (pulse) model match {
        case ExecutionModel.TrappedIon(p) =>
          val zones = qs.map(q => p.zoneByQubit.getOrElse(q, 0)).distinct
          resources = zones.map(z => s"zone:$z" -> 1).toMap + ("laser" -> p.laserChannels)
          if (qs.size >= 2) setup = p.coolingNs
          if (zones.size > 1) {
            val leg = Math.addExact(Math.addExact(p.splitNs, p.shuttleNs), p.mergeNs)
            setup = Math.addExact(setup, Math.multiplyExact(2L, leg))
            resources += "transport" -> p.transportChannels
            logSurvival += 2.0 * math.log1p(-p.transportErrorPerLeg)
            if (leg == 0L) warnings += "Cross-zone ion transport time is missing; timing and fidelity are incomplete."
          }
        case ExecutionModel.NeutralAtom(p) =>
          resources = Map("addressing" -> p.maxParallelGates)
          val outOfRange = for {
            radius <- p.blockadeRadiusUm
            if qs.size >= 2 && qs.forall(p.positions.contains)
          } yield qs.combinations(2).exists { pair =>
            p.positions(pair(0)).distanceTo(p.positions(pair(1))) + 2.0 * p.positionUncertaintyUm > radius
          }
          if (outOfRange.contains(true)) {
            exclusive = true
            setup = p.roundTripMoveNs.getOrElse(0L)
            logSurvival += math.log(p.moveSurvival)
            if (p.roundTripMoveNs.isEmpty)
              warnings += "Out-of-range atom interaction has no round-trip movement time; timing and fidelity are incomplete."
          }
          val reloadCount = qs.count(measured.contains)
          if (reloadCount > 0) {
            exclusive = true
            setup = Math.addExact(setup, p.reloadNs.getOrElse(0L))
            logSurvival += reloadCount.toDouble * math.log(p.reloadSurvival)
            if (p.reloadNs.isEmpty)
              warnings += "Atom reuse after measurement has no reload/rearrangement time; timing and fidelity are incomplete."
          }
        case _ => ()
      }
      if (measurement) measured ++= qs
      else if (pulse) measured --= qs
      Operation(index, qs, duration, setup, measurement, resources, exclusive, logSurvival)
    }.toVector

    def spatialConflict(a: Operation, b: Operation, p: NeutralAtomParallelism): Boolean = {
      val interactionRadius = if (p.requireInteractionExclusionForAllGates) p.interactionExclusionRadiusUm
        else p.interactionExclusionRadiusUm.orElse(p.blockadeRadiusUm)
      val radius = if (p.requireInteractionExclusionForAllGates || a.qubits.size >= 2 || b.qubits.size >= 2)
        for { interaction <- interactionRadius; address <- p.addressingExclusionRadiusUm } yield interaction.max(address)
      else p.addressingExclusionRadiusUm
      radius match {
        case None => true
        case Some(r) =>
          (a.qubits ++ b.qubits).exists(q => !p.positions.contains(q)) ||
            a.qubits.exists(x => b.qubits.exists(y =>
              p.positions(x).distanceTo(p.positions(y)) - 2.0 * p.positionUncertaintyUm <= r))
      }
    }

    def hardConflict(a: Operation, b: Operation, serial: Boolean): Boolean = {
      if (a.exclusive || b.exclusive) true
      else if (a.measurement && b.measurement) false
      else if (a.measurement || b.measurement) blocksMeasurement
      else if (serial || model == ExecutionModel.GlobalSerialGates) true
      else model match {
        case ExecutionModel.NeutralAtom(p) => spatialConflict(a, b, p)
        case _ => false
      }
    }

    def overlapLog(a: TimedGate, b: TimedGate): Double = {
      val overlap = math.min(a.endNs, b.endNs) - math.max(a.gateStartNs, b.gateStartNs)
      val shorter = math.min(a.endNs - a.gateStartNs, b.endNs - b.gateStartNs)
      if (a.isMeasurement || b.isMeasurement || overlap <= 0L || shorter <= 0L) 0.0
      else {
        val fullOverlapLog = model match {
          case ExecutionModel.TrappedIon(p) => math.log1p(-p.concurrentGateError.getOrElse(0.0))
          case ExecutionModel.NeutralAtom(p) =>
            (p.laserWaistUm, p.crosstalkErrorAtZeroDistance) match {
              case (Some(waist), Some(error)) =>
                (for {
                  x <- a.qubits
                  y <- b.qubits
                  px <- p.positions.get(x)
                  py <- p.positions.get(y)
                } yield {
                  val distance = (px.distanceTo(py) - 2.0 * p.positionUncertaintyUm).max(0.0)
                  math.log1p(-error * math.exp(-2.0 * math.pow(distance / waist, 2.0)))
                }).sum
              case _ => 0.0
            }
          case _ => 0.0
        }
        fullOverlapLog * (overlap.toDouble / shorter.toDouble)
      }
    }

    def build(mode: Int): HardwareSchedule = {
      // 0: earliest feasible; 1: locally minimize exposure; 2: serialize gates.
      val reservations = mutable.ArrayBuffer.empty[Reservation]
      val available = mutable.Map.empty[Int, Long].withDefaultValue(0L)
      var globalGateEnd = 0L
      var measurementEnd = 0L
      var logCross = 0.0
      operations.foreach { op =>
        val isPulse = !op.measurement && op.durationNs > 0L && op.qubits.nonEmpty
        val localStart = op.qubits.map(available).maxOption.getOrElse(0L)
        val gateFloor = if (model == ExecutionModel.GlobalSerialGates && (isPulse || op.measurement)) globalGateEnd else 0L
        val measurementFloor = if (blocksMeasurement) {
          if (op.measurement) globalGateEnd else if (isPulse) measurementEnd else 0L
        } else 0L
        val floor = localStart.max(gateFloor).max(measurementFloor)
        val length = Math.addExact(op.setupNs, op.durationNs)
        def at(start: Long): TimedGate = TimedGate(op.index, op.qubits, start,
          Math.addExact(start, op.setupNs), Math.addExact(start, length), op.resources.keys.toVector.sorted, op.measurement)

        def earliest(from: Long): Long = {
          var start = from
          var searching = length > 0L && hasHardwareResources
          while (searching) {
            val end = Math.addExact(start, length)
            val overlapping = reservations.filter(r => r.timed.endNs > start && r.timed.startNs < end && r.timed.endNs > r.timed.startNs)
            val blockers = overlapping.filter(r => hardConflict(op, r.op, mode == 2))
            // Check interval capacity at every change within this proposed reservation;
            // counting all overlapping intervals at once would falsely serialize gaps.
            val capacityReleases = op.resources.toVector.flatMap { case (resource, capacity) =>
              val users = overlapping.filter(_.op.resources.contains(resource))
              val events = (Vector(start) ++ users.map(_.timed.startNs).filter(_ > start)).distinct.sorted
              events.iterator.flatMap { t =>
                val active = users.filter(r => r.timed.startNs <= t && r.timed.endNs > t)
                if (active.size >= capacity) Some(active.map(_.timed.endNs).min) else None
              }.take(1).toVector
            }
            val releases = blockers.map(_.timed.endNs).toVector ++ capacityReleases
            if (releases.isEmpty) searching = false
            else start = releases.min // Always advances to an observed resource release.
          }
          start
        }

        val first = earliest(floor)
        def crossAt(start: Long): Double = {
          val current = at(start)
          if (hasSoftNoise) reservations.iterator.map(r => overlapLog(current, r.timed)).sum else 0.0
        }
        val chosen = if (mode == 1 && isPulse) {
          val firstEnd = Math.addExact(first, length)
          val releases = reservations.iterator
            .filter(r => !r.op.measurement && r.timed.endNs > first && r.timed.startNs < firstEnd)
            .map(_.timed.endNs).toVector.distinct.sorted
          val candidates = (Vector(first) ++ releases.map(earliest)).distinct
          val rate = op.qubits.map(q => DecoherenceEstimator.infidelityRate(q, cal)).sum / 1e9
          candidates.minBy { start =>
            val waitingCost = if (start == first) 0.0 else (start - first).toDouble * rate
            (waitingCost - crossAt(start), start)
          }
        } else first
        val timed = at(chosen)
        logCross += crossAt(chosen)
        reservations += Reservation(op, timed)
        op.qubits.foreach(q => available(q) = timed.endNs)
        if (isPulse) globalGateEnd = globalGateEnd.max(timed.endNs)
        if (op.measurement) measurementEnd = measurementEnd.max(timed.endNs)
      }
      val timed = reservations.map(_.timed).toVector
      val gates = timed.filter(g => !g.isMeasurement && g.endNs > g.gateStartNs)
      val gateSpan = gates.map(_.endNs).maxOption.getOrElse(0L)
      val events = gates.flatMap(g => Vector(g.gateStartNs -> 1, g.endNs -> -1))
        .groupMapReduce(_._1)(_._2)(_ + _).toVector.sortBy(_._1)
      val peak = events.scanLeft(0)((n, e) => n + e._2).max
      val decoherence = DecoherenceEstimator.estimate(ops, timed, cal)
      HardwareSchedule(
        operations = timed,
        perQubitEndTimeSec = available.iterator.map { case (q, t) => q -> (t.toDouble / 1e9) }.toMap,
        makespanNs = timed.map(_.endNs).maxOption.getOrElse(0L),
        averageGateParallelism = if (gateSpan == 0L) 0.0 else gates.map(g => (g.endNs - g.gateStartNs).toDouble).sum / gateSpan.toDouble,
        peakGateParallelism = peak,
        logPCrosstalk = logCross,
        logPTransport = operations.map(_.logSurvival).sum,
        warnings = (warnings.toVector ++ decoherence.warnings).distinct,
        decoherence = decoherence
      )
    }

    val candidates = if (hasSoftNoise) Vector(build(0), build(1), build(2)) else Vector(build(0))
    // Exact comparison under the scoring model, including waiting propagated to
    // dependent gates and terminal measurement. Ties prefer the shorter schedule.
    candidates.minBy(s => (-s.decoherence.logP - s.logPCrosstalk - s.logPTransport, s.makespanNs))
  }
}
