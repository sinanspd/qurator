package qurator.testbed.fidelity

import qurator.domain.calibration._
import qurator.domain.circuit._

import scala.collection.mutable

/** Sampled error frames, not state-vector simulation or a hardware-fidelity oracle.
  * Exact Pauli propagation for Clifford gates; a local Pauli twirl after non-Clifford gates.
  * See benchmark-fidelity.md for the score, noise assumptions and coverage limitations.
  */
final class PauliTraceEvaluator(val settings: PauliTraceEvaluator.Settings = PauliTraceEvaluator.Settings())
    extends BenchmarkFidelityEvaluator {
  import PauliTraceEvaluator._

  val id = "pauli"

  def evaluate(compiled: Circuit, calibration: CanonicalCalibration): FidelityEvaluation = {
    val trace = prepare(compiled, calibration)
    // Each evaluation gets its own RNG: results do not depend on policy traversal order or concurrency.
    val random = new java.util.Random(settings.seed)
    val frame = new Array[Int](trace.width)
    var successes = 0
    var shot = 0
    while (shot < settings.samples) {
      java.util.Arrays.fill(frame, 0)
      var alive = true
      var i = 0
      while (i < trace.steps.length && alive) {
        trace.steps(i) match {
          case Unitary(qubits, transfer, faultProbability) =>
            var code = 0
            var j = 0
            while (j < qubits.length) { code |= frame(qubits(j)) << (2 * j); j += 1 }
            // Identity always remains identity; this is common in high-fidelity traces.
            var evolved = if (code == 0) 0 else transfer.rows(code).sample(random)
            if (faultProbability > 0 && random.nextDouble() < faultProbability)
              evolved ^= 1 + random.nextInt((1 << (2 * qubits.length)) - 1)
            j = 0
            while (j < qubits.length) { frame(qubits(j)) = (evolved >> (2 * j)) & 3; j += 1 }
          case Memory(q, px, py, pz) =>
            val draw = random.nextDouble()
            if (draw < px) frame(q) ^= 1
            else if (draw < px + py) frame(q) ^= 3
            else if (draw < px + py + pz) frame(q) ^= 2
          case Checkpoint(q, readoutFidelity) =>
            // This is a process-error checkpoint, not a simulated output bit or Hellinger fidelity.
            alive = frame(q) == 0 && (readoutFidelity == 1 || random.nextDouble() < readoutFidelity)
            frame(q) = 0
          case Clear(q) => frame(q) = 0
          case Preparation(q, survival) =>
            if (survival < 1 && random.nextDouble() >= survival) frame(q) ^= 1
        }
        i += 1
      }
      if (alive && frame.forall(_ == 0)) successes += 1
      shot += 1
    }
    val score = successes.toDouble / settings.samples
    val resolutionWarning = Option.when(successes == 0)(
      "No surviving samples: the score is below sampling resolution, not evidence of exactly zero fidelity.")
    FidelityEvaluation(id, "pauli_identity_survival_proxy", score, math.log(score),
      samples = Some(settings.samples), seed = Some(settings.seed),
      confidence95 = Some(wilson95(successes, settings.samples)),
      approximatedGates = trace.approximatedGates,
      warnings = trace.warnings ++ resolutionWarning)
  }

  private def prepare(circuit: Circuit, cal: CanonicalCalibration): Trace = {
    val issues = mutable.LinkedHashMap.empty[String, Int]
    def issue(message: String): Unit = issues.update(message, issues.getOrElse(message, 0) + 1)
    val assumptions = mutable.LinkedHashSet.empty[String]
    val serial = cal.executionModel match {
      case ExecutionModel.ParallelByQubitOrEdge => false
      case ExecutionModel.GlobalSerialGates => true
      case other => throw new IllegalArgumentException(
        s"Pauli evaluator does not implement resource/transport model $other; use legacy or a supported gate profile")
    }
    require(cal.memoryInfidelityRatePerSecond.isEmpty && cal.pasqalTiming.isEmpty,
      "Pauli evaluator requires gate-based T1/T2 calibration, not an analytical/analog memory model")

    val physical = circuit.remainingGates.iterator.flatMap(gateKey(_)._2).toSet.toVector.sorted
    require(physical.forall(_ >= 0), "Negative physical qubit index")
    val dense = physical.zipWithIndex.toMap
    val end = new Array[Long](physical.size)
    val active = Array.fill(physical.size)(true)
    var globalEnd = 0L
    var gateResourceEnd = 0L
    val steps = mutable.ArrayBuffer.empty[Step]
    var approximatedGates = 0
    // Cache is local to the call and bounded; parameter-rich MQT traces cannot grow it indefinitely.
    val transfers = mutable.LinkedHashMap.empty[(GateTimingKey, Int), PauliTransfer.Transfer]

    val hasTiming = circuit.remainingGates.forall { gate =>
      val found = cal.durationNsOptionFor(gate).isDefined
      if (!found) issue(s"Missing duration for ${gateKey(gate)._1}; temporal noise disabled for this circuit")
      found
    }
    def probability(value: Option[Double]): Option[Double] =
      value.filter(p => p.isFinite && p >= 0 && p <= 1)
    val preparation = probability(cal.initSurvivalPerQubit).getOrElse {
      if (cal.initSurvivalPerQubit.nonEmpty) issue("Invalid state-preparation fidelity; preparation assumed ideal")
      else assumptions += "State preparation and reset assumed ideal unless initSurvivalPerQubit is supplied."
      1.0
    }
    physical.indices.foreach(q => if (preparation < 1) steps += Preparation(q, preparation))

    def memory(q: Int, durationNs: Long): Unit = if (hasTiming && durationNs > 0) {
      val t1 = cal.t1For(physical(q))
      val reportedT2 = cal.t2For(physical(q))
      if (t1.isEmpty) issue("Missing T1 for exposed qubits; relaxation omitted")
      if (reportedT2.isEmpty) issue("Missing T2 for exposed qubits; only known T1 relaxation included")
      val t2 = (t1, reportedT2) match {
        case (Some(a), Some(b)) if b > 2 * a =>
          issue("T2 exceeds 2*T1; T2 capped at the completely-positive thermal-channel limit")
          Some(2 * a)
        case _ => reportedT2
      }
      val seconds = durationNs.toDouble * 1e-9
      val lz = t1.fold(1.0)(t => math.exp(-seconds / t))
      val lx = t2.fold(math.sqrt(lz))(t => math.exp(-seconds / t))
      val px = (1 - lz) / 4
      val pz = math.max(0, (1 + lz - 2 * lx) / 4) // only protects roundoff after T2 capping
      if (px > 0 || pz > 0) steps += Memory(q, px, px, pz)
    }

    circuit.remainingGates.foreach { gate =>
      val (name, rawQubits) = gateKey(gate)
      require(rawQubits.distinct.size == rawQubits.size, s"Repeated operand in $gate")
      val qubits = rawQubits.map(dense).toArray
      val barrier = name == "BARRIER"
      val globalPhase = gate.isInstanceOf[GPhase] || name == "GPHASE"
      val gatePulse = !gate.isInstanceOf[Measure] && !barrier && !globalPhase
      val qs = if (barrier && qubits.isEmpty) physical.indices.toArray else qubits
      val duration = if (hasTiming) cal.durationNsOptionFor(gate).get else 0L
      val dependencyEnd = qs.foldLeft(0L)((t, q) => math.max(t, end(q)))
      // GlobalSerialGates serializes gate pulses, not independent readout operations.
      val start = if (serial && gatePulse) math.max(gateResourceEnd, dependencyEnd) else dependencyEnd
      val finish = Math.addExact(start, duration)

      if (!globalPhase) {
        qs.foreach(q => if (active(q)) memory(q, start - end(q)))
        gate match {
          case Measure(_) =>
            val q = qubits(0)
            val readout = probability(cal.readoutFidelity.get(rawQubits(0)))
              .orElse(probability(cal.readoutFidelityAvg)).getOrElse {
                issue("Missing readout fidelity; readout noise omitted")
                1.0
              }
            steps += Checkpoint(q, readout)
            active(q) = false
          case Reset(_) =>
            val q = qubits(0)
            steps += Clear(q)
            if (preparation < 1) steps += Preparation(q, preparation)
            active(q) = true
            assumptions += "Resets discard incoming Pauli errors; reset duration uses the calibration's one-qubit timing."
          case _ if barrier => ()
          case _ =>
            require(qubits.forall(active(_)), s"Gate after measurement without reset is unsupported: $gate")
            if (name == "GPI" || name == "GPI2" || name == "ZZ") assumptions +=
              "Native GPI/GPI2 phases use turns and ZZ angles use radians, matching DeviceFitBenchmark's internal convention."
            val key = PauliTransfer.key(gate)
            val transfer = transfers.getOrElseUpdate(key, PauliTransfer.build(gate))
            if (transfers.size > 4096) transfers.remove(transfers.head._1)
            if (transfer.approximated) approximatedGates += 1
            val eps = cal.epsOptionFor(gate).getOrElse {
              issue(s"Missing ${qubits.length}-qubit gate error; that gate's stochastic noise omitted")
              0.0
            }
            val faultProbability = settings.errorConvention.probability(eps, qubits.length)
            require(faultProbability >= 0 && faultProbability <= 1,
              s"Gate error $eps cannot be interpreted as ${settings.errorConvention.name} for $gate")
            steps += Unitary(qubits, transfer, faultProbability)
            if (!cal.gateErrorsIncludeDecoherence) qubits.foreach(q => memory(q, duration))
        }
        qs.foreach(q => end(q) = finish)
        globalEnd = math.max(globalEnd, finish)
        if (gatePulse && duration > 0L) gateResourceEnd = math.max(gateResourceEnd, finish)
      }
    }
    physical.indices.foreach(q => if (active(q)) memory(q, globalEnd - end(q)))
    if (settings.strictCalibration && issues.nonEmpty)
      throw new IllegalArgumentException("Incomplete Pauli calibration: " + issues.keys.mkString("; "))
    if (approximatedGates > 0) assumptions +=
      "Local Pauli twirling at non-Clifford gates discards coherent interference; this is an approximation."
    assumptions += s"Gate errors interpreted as ${settings.errorConvention.name}; independent symmetric Pauli noise."
    assumptions += "No coherent drift, crosstalk, leakage or correlated faults; confidence bounds cover sampling only."
    Trace(steps.toArray, physical.size, approximatedGates,
      cal.warnings ++ issues.iterator.map { case (message, n) => s"$message ($n occurrence(s))" } ++ assumptions)
  }
}

object PauliTraceEvaluator {
  sealed trait ErrorConvention {
    def name: String
    def probability(error: Double, arity: Int): Double
  }
  case object AverageInfidelity extends ErrorConvention {
    val name = "average-infidelity"
    def probability(error: Double, arity: Int): Double = error * ((1 << arity) + 1.0) / (1 << arity)
  }
  case object PauliProbability extends ErrorConvention {
    val name = "pauli-probability"
    def probability(error: Double, arity: Int): Double = error
  }
  final case class Settings(
      samples: Int = 2048,
      seed: Long = 42L,
      strictCalibration: Boolean = false,
      errorConvention: ErrorConvention = AverageInfidelity
  ) {
    require(samples > 0, "Pauli sample count must be positive")
  }

  private sealed trait Step
  private case class Unitary(qubits: Array[Int], transfer: PauliTransfer.Transfer, faultProbability: Double) extends Step
  private case class Memory(q: Int, px: Double, py: Double, pz: Double) extends Step
  private case class Checkpoint(q: Int, readoutFidelity: Double) extends Step
  private case class Clear(q: Int) extends Step
  private case class Preparation(q: Int, survival: Double) extends Step
  private case class Trace(steps: Array[Step], width: Int, approximatedGates: Int, warnings: Vector[String])

  private[fidelity] def wilson95(successes: Int, n: Int): (Double, Double) = {
    val z = 1.959963984540054
    val p = successes.toDouble / n
    val denom = 1 + z * z / n
    val center = (p + z * z / (2 * n)) / denom
    val radius = z * math.sqrt(p * (1 - p) / n + z * z / (4.0 * n * n)) / denom
    (math.max(0, center - radius), math.min(1, center + radius))
  }
}
