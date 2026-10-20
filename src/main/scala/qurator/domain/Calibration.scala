package qurator.domain

import qurator.domain.circuit._

object calibration {

    sealed trait DeviceCalibration 

    /** A Pasqal Device snapshot, not a synthetic gate-model calibration. Rates are /microsecond. */
    case class PasqalNoiseCalibration(
        statePrepError: Option[Double], falsePositive: Option[Double], falseNegative: Option[Double],
        relaxationRate: Option[Double], dephasingRate: Option[Double]
    ) {
        require(List(statePrepError, falsePositive, falseNegative).flatten.forall(validProbability), "Invalid Pasqal SPAM probability")
        require(List(relaxationRate, dephasingRate).flatten.forall(x => x.isFinite && x >= 0), "Invalid Pasqal noise rate")
        def t1Seconds: Option[Double] = relaxationRate.filter(_ > 0).map(1e-6 / _)
        // Pulser's dephasing collapse operator supplies pure transverse decay.
        def t2Seconds: Option[Double] = for {
            r <- relaxationRate; d <- dephasingRate; rate = r / 2 + d if rate > 0
        } yield 1e-6 / rate
        def memoryInfidelityRatePerSecond: Option[Double] = for {
            r <- relaxationRate; d <- dephasingRate
            rate = 1e6 * (r / 3 + d / 3) if rate.isFinite
        } yield rate
        def readoutFidelity: Option[Double] = for { p <- falsePositive; n <- falseNegative } yield 1 - (p + n) / 2
    }

    /** A reusable timing template, independent of which runtime circuit uses it. */
    case class GateTimingKey(name: String, parameters: Vector[String] = Vector.empty)
    case class PasqalTimingModel(
        fixedDurationNs: Map[GateTimingKey, Long] = Map.empty,
        rabiFrequencyRadPerUs: Option[Double] = None,
        useSquarePulseRotations: Boolean = false,
        pulseOverheadNs: Long = 0L,
        source: String = "User-configured timing templates"
    ) {
        require(fixedDurationNs.forall { case (k, t) => k.name.nonEmpty && t >= 0 }, "Invalid timing template")
        require(rabiFrequencyRadPerUs.forall(x => x.isFinite && x > 0), "Rabi frequency must be positive and finite")
        require(!useSquarePulseRotations || rabiFrequencyRadPerUs.nonEmpty, "Square-pulse timing requires an operating Rabi frequency")
        require(pulseOverheadNs >= 0 && source.trim.nonEmpty, "Timing assumptions need nonnegative overhead and a source")
    }
    /** Optional configuration is explicit estimation input, not claimed provider calibration. */
    case class PasqalEstimationConfig(
        timing: PasqalTimingModel = PasqalTimingModel(),
        positions: Map[Int, AtomPosition] = Map.empty,
        interactionCoeffRadPerUsUm6: Option[Double] = None,
        residualInteractionTolerance: Option[Double] = None,
        addressingExclusionRadiusUm: Option[Double] = None,
        independentGateCapacity: Int = 1,
        positionUncertaintyUm: Double = 0.0
    ) {
        require(positions.keys.forall(_ >= 0), "Invalid atom index")
        require(interactionCoeffRadPerUsUm6.forall(x => x.isFinite && x != 0), "Invalid interaction coefficient")
        require(residualInteractionTolerance.forall(x => x.isFinite && x > 0 && x <= 1), "Interaction tolerance must be in (0, 1]")
        require(addressingExclusionRadiusUm.forall(x => x.isFinite && x >= 0), "Invalid addressing exclusion radius")
        require(independentGateCapacity > 0, "Independent gate capacity must be positive")
        require(positionUncertaintyUm.isFinite && positionUncertaintyUm >= 0, "Invalid position uncertainty")
    }
    case class PasqalCalibration(
        deviceName: String, noise: PasqalNoiseCalibration,
        timing: PasqalTimingModel = PasqalTimingModel(),
        parallelism: NeutralAtomParallelism = NeutralAtomParallelism(requireInteractionExclusionForAllGates = true),
        readoutDurationNs: Option[Long] = None,
        warnings: Vector[String] = Vector.empty,
        assumptions: Vector[String] = Vector.empty
    ) extends DeviceCalibration {
        require(readoutDurationNs.forall(_ >= 0), "Invalid readout duration")
    }

    sealed trait ExecutionModel
    object ExecutionModel {
        case object ParallelByQubitOrEdge extends ExecutionModel
        case object GlobalSerialGates extends ExecutionModel
        final case class TrappedIon(resources: IonTrapParallelism) extends ExecutionModel
        final case class NeutralAtom(resources: NeutralAtomParallelism) extends ExecutionModel
    }

    /** Resources of a gate-model ion device, not inferred from all-to-all connectivity.
      * Cross-zone gates use a conservative round trip to the home zones. Times are ns.
      * Transport/overlap errors must exclude memory loss modeled by T1/T2 separately.
      */
    case class IonTrapParallelism(
        gateZones: Int = 1,
        laserChannels: Int = 1,
        zoneByQubit: Map[Int, Int] = Map.empty,
        transportChannels: Int = 1,
        splitNs: Long = 0L,
        shuttleNs: Long = 0L,
        mergeNs: Long = 0L,
        coolingNs: Long = 0L,
        transportErrorPerLeg: Double = 0.0,
        concurrentGateError: Option[Double] = None,
        measurementBlocksGates: Boolean = true
    ) {
        require(gateZones > 0 && laserChannels > 0 && transportChannels > 0, "Resource counts must be positive")
        require(zoneByQubit.forall { case (q, z) => q >= 0 && z >= 0 && z < gateZones }, "Invalid ion zone assignment")
        require(List(splitNs, shuttleNs, mergeNs, coolingNs).forall(_ >= 0L), "Durations must be nonnegative")
        require(validProbability(transportErrorPerLeg), "Invalid transport error")
        require(concurrentGateError.forall(validProbability), "Invalid concurrent gate error")
    }

    case class AtomPosition(xUm: Double, yUm: Double) {
        require(xUm.isFinite && yUm.isFinite, "Atom coordinates must be finite, in micrometres")
        def distanceTo(other: AtomPosition): Double = math.hypot(xUm - other.xUm, yUm - other.yUm)
    }

    /** Physical-qubit positions after placement. None means unknown, not zero.
      * Exclusion uses distance minus twice position uncertainty. Motion/reload blocks gates.
      * Survival and overlap errors describe additional loss beyond the T1/T2 memory model.
      */
    case class NeutralAtomParallelism(
        positions: Map[Int, AtomPosition] = Map.empty,
        blockadeRadiusUm: Option[Double] = None,
        addressingExclusionRadiusUm: Option[Double] = None,
        maxParallelGates: Int = 1,
        positionUncertaintyUm: Double = 0.0,
        laserWaistUm: Option[Double] = None,
        crosstalkErrorAtZeroDistance: Option[Double] = None,
        roundTripMoveNs: Option[Long] = None,
        moveSurvival: Double = 1.0,
        reloadNs: Option[Long] = None,
        reloadSurvival: Double = 1.0,
        measurementBlocksGates: Boolean = true,
        // Distinct from blockadeRadiusUm, which controls intra-gate reach.
        interactionExclusionRadiusUm: Option[Double] = None,
        requireInteractionExclusionForAllGates: Boolean = false
    ) {
        require(positions.keys.forall(_ >= 0), "Invalid atom index")
        require(maxParallelGates > 0, "Parallel gate capacity must be positive")
        require(List(blockadeRadiusUm, laserWaistUm, interactionExclusionRadiusUm).flatten.forall(x => x.isFinite && x > 0.0), "Radii must be positive and finite")
        require(addressingExclusionRadiusUm.forall(x => x.isFinite && x >= 0.0), "Invalid addressing radius")
        require(positionUncertaintyUm.isFinite && positionUncertaintyUm >= 0.0, "Invalid position uncertainty")
        require(crosstalkErrorAtZeroDistance.forall(validProbability), "Invalid crosstalk error")
        require(roundTripMoveNs.forall(_ > 0L) && reloadNs.forall(_ > 0L), "Motion/reload durations must be positive")
        require(validProbability(moveSurvival) && validProbability(reloadSurvival), "Invalid survival probability")
    }

    private def validProbability(p: Double): Boolean = p.isFinite && p >= 0.0 && p <= 1.0

    case class CalibrationTopology(
        qubits: List[Int],
        edges: List[(Int, Int)]
    ) {
        def normalizedEdges: List[(Int, Int)] =
            edges.map { case (a, b) => if (a <= b) (a, b) else (b, a) }.distinct.sorted
    }

    case class QubitCalibrationMetrics(
        t1Seconds: Option[Double] = None,
        t2Seconds: Option[Double] = None,
        oneQubitFidelity: Option[Double] = None,
        readoutFidelity: Option[Double] = None,
        readoutError: Option[Double] = None,
        probMeasu0Prep1: Option[Double] = None,
        probMeasu1Prep0: Option[Double] = None,
        gateErrors: Map[String, Double] = Map.empty,
        gateFidelities: Map[String, Double] = Map.empty,
        gateDurationsNs: Map[String, Long] = Map.empty
    )

    case class EdgeCalibrationMetrics(
        gateErrors: Map[String, Double] = Map.empty,
        gateFidelities: Map[String, Double] = Map.empty,
        gateDurationsNs: Map[String, Long] = Map.empty
    )

    case class AQTCalibration(
        t1Seconds: Double, 
        t2Seconds: Double,
        readoutFidelity: Double, 
        readoutDurationSec: Double,
        oneQGateDurationSec: Double,
        oneQGateFidelity: Double, 
        twoQGateDurationSec: Double,
        twoQGateFidelity: Double,
        topology: Option[CalibrationTopology] = None,
        qubitMetrics: Map[Int, QubitCalibrationMetrics] = Map.empty,
        edgeMetrics: Map[(Int, Int), EdgeCalibrationMetrics] = Map.empty,
        updatedAt: Option[String] = None,
        parallelism: Option[IonTrapParallelism] = None
    ) extends DeviceCalibration

    case class IBMCalibration(
        qubits: List[Int],
        edges: List[(Int, Int)],
        t1Seconds: List[(Int, Double)],
        t2Seconds: List[(Int, Double)],
        t1AvgSeconds: Double,
        t2AvgSeconds: Double,
        probMeasu0Presp1: Double,
        probMeasu1Presp0: Double,
        idError: Double,
        rxError: Double,
        pauliXError: Double,
        czError: Double,
        rzzError: Double,
        readoutLengthNs: Long,
        singleQGateLengthNs: Long,
        idLengthNs: Long,
        twoQGateLengthNs: Long,
        czGateLengthNs: Long,
        rzzGateLengthNs: Long,
        topology: Option[CalibrationTopology] = None,
        qubitMetrics: Map[Int, QubitCalibrationMetrics] = Map.empty,
        edgeMetrics: Map[(Int, Int), EdgeCalibrationMetrics] = Map.empty,
        basisGates: List[String] = Nil,
        calibrationId: Option[String] = None,
        updatedAt: Option[String] = None
    ) extends DeviceCalibration

    case class IonQCalibration(
        t1Seconds: Double,
        t2Seconds: Double,
        avg1qFidelityPct: Double,
        avg2qFidelityPct: Double, 
        avgReadoutFidelity: Double,
        oneQGateDurationSec: Double,
        twoQGateDurationSec: Double,
        readoutDurationSec: Double,
        topology: Option[CalibrationTopology] = None,
        qubitMetrics: Map[Int, QubitCalibrationMetrics] = Map.empty,
        edgeMetrics: Map[(Int, Int), EdgeCalibrationMetrics] = Map.empty,
        updatedAt: Option[String] = None,
        parallelism: Option[IonTrapParallelism] = None,
        warnings: Vector[String] = Vector.empty
    ) extends DeviceCalibration

    case class IQMCalibration(
        t1: Double,
        t2: Double, 
        q1fidelity: Double,
        q2fidelity: Double, 
        readoutFidelity: Double,
        topology: Option[CalibrationTopology] = None,
        qubitMetrics: Map[Int, QubitCalibrationMetrics] = Map.empty,
        edgeMetrics: Map[(Int, Int), EdgeCalibrationMetrics] = Map.empty,
        updatedAt: Option[String] = None,
        // Optional explicit timings; absent provider values remain unknown.
        oneQGateDurationNs: Option[Long] = None,
        twoQGateDurationNs: Option[Long] = None,
        readoutDurationNs: Option[Long] = None,
        warnings: Vector[String] = Vector.empty
        // typicalDetectionFalsePositive: Double,
        // typicalDetectionFalseNegative: Double,
        // typicalVacancyError: Option[Double],     
        // typicalFillingError: Option[Double],       
        // typicalAtomLossProbability: Option[Double],
        // t1SingleSec: Option[Double],
        // t2EchoSingleSec: Option[Double],
        // t2SingleSec: Option[Double]
    ) extends DeviceCalibration

    case class QuEraCalibration(
        typicalDetectionFalsePositive: Double,
        typicalDetectionFalseNegative: Double,
        typicalVacancyError: Option[Double],     
        typicalFillingError: Option[Double],       
        typicalAtomLossProbability: Option[Double],
        t1SingleSec: Option[Double],
        t2EchoSingleSec: Option[Double],
        t2SingleSec: Option[Double]
    ) extends DeviceCalibration

    /** Explicit gate-model calibration; Aquila AHS pulse limits are not gate durations. */
    case class NeutralAtomCalibration(
        oneQGateError: Double,
        twoQGateError: Double,
        readoutFidelity: Double,
        oneQGateDurationNs: Long,
        twoQGateDurationNs: Long,
        readoutDurationNs: Long,
        t1Seconds: Option[Double],
        t2Seconds: Option[Double],
        parallelism: NeutralAtomParallelism,
        initSurvivalPerQubit: Option[Double] = None,
        topology: Option[CalibrationTopology] = None
    ) extends DeviceCalibration {
        require(validProbability(oneQGateError) && validProbability(twoQGateError), "Invalid gate error")
        require(readoutFidelity.isFinite && readoutFidelity >= 0.0 && readoutFidelity <= 1.0, "Invalid readout fidelity")
        require(oneQGateDurationNs > 0 && twoQGateDurationNs > 0 && readoutDurationNs >= 0, "Invalid gate-model timing")
        require(List(t1Seconds, t2Seconds).flatten.forall(t => t.isFinite && t > 0), "Invalid coherence time")
        require(initSurvivalPerQubit.forall(p => p.isFinite && p >= 0 && p <= 1), "Invalid initial survival")
    }

    case class RigettiCalibration(
        t1Seconds: Double,
        t2Seconds: Double,
        avg1qFidelityPct: Double,
        readoutFidelityPct: Double,
        swapFidelityPct: Double,
        oneQGateDurationNs: Long,
        twoQGateDurationNs: Long,
        swapGateDurationNs: Long,
        readoutDurationNs: Long,
        topology: Option[CalibrationTopology] = None,
        qubitMetrics: Map[Int, QubitCalibrationMetrics] = Map.empty,
        edgeMetrics: Map[(Int, Int), EdgeCalibrationMetrics] = Map.empty,
        updatedAt: Option[String] = None
    ) extends DeviceCalibration

    def topologyOf(c: DeviceCalibration): Option[CalibrationTopology] =
        c match {
            case a: IBMCalibration =>
                a.topology.orElse {
                    if (a.qubits.nonEmpty || a.edges.nonEmpty) Some(CalibrationTopology(a.qubits, a.edges))
                    else None
                }
            case a: AQTCalibration =>
                a.topology
            case a: IonQCalibration =>
                a.topology
            case a: IQMCalibration =>
                a.topology
            case a: RigettiCalibration =>
                a.topology
            case a: PasqalCalibration =>
                val p = a.parallelism
                p.blockadeRadiusUm.filter(_ => p.positions.nonEmpty).map { r =>
                    val qs = p.positions.keys.toList.sorted
                    val edges = qs.combinations(2).collect {
                        case List(x, y) if p.positions(x).distanceTo(p.positions(y)) + 2 * p.positionUncertaintyUm <= r => x -> y
                    }.toList
                    CalibrationTopology(qs, edges)
                }
            case a: NeutralAtomCalibration =>
                a.topology.orElse {
                    val p = a.parallelism
                    if (p.positions.isEmpty || (p.blockadeRadiusUm.isEmpty && p.roundTripMoveNs.isEmpty)) None
                    else {
                        val qubits = p.positions.keys.toList.sorted
                        val edges = qubits.combinations(2).collect {
                            case List(x, y) if p.roundTripMoveNs.nonEmpty || p.blockadeRadiusUm.exists { r =>
                                p.positions(x).distanceTo(p.positions(y)) + 2.0 * p.positionUncertaintyUm <= r
                            } => x -> y
                        }.toList
                        Some(CalibrationTopology(qubits, edges))
                    }
                }
            case _ =>
                None
        }

    def qubitMetricsOf(c: DeviceCalibration): Map[Int, QubitCalibrationMetrics] =
        c match {
            case a: IBMCalibration     => a.qubitMetrics
            case a: AQTCalibration     => a.qubitMetrics
            case a: IonQCalibration    => a.qubitMetrics
            case a: IQMCalibration     => a.qubitMetrics
            case a: RigettiCalibration => a.qubitMetrics
            case _                     => Map.empty
        }

    def edgeMetricsOf(c: DeviceCalibration): Map[(Int, Int), EdgeCalibrationMetrics] =
        c match {
            case a: IBMCalibration =>
                a.edgeMetrics.map { case (edge, metrics) =>
                    val normalized = if (edge._1 <= edge._2) edge else edge.swap
                    normalized -> metrics
                }
            case a: AQTCalibration =>
                a.edgeMetrics.map { case (edge, metrics) =>
                    val normalized = if (edge._1 <= edge._2) edge else edge.swap
                    normalized -> metrics
                }
            case a: IonQCalibration =>
                a.edgeMetrics.map { case (edge, metrics) =>
                    val normalized = if (edge._1 <= edge._2) edge else edge.swap
                    normalized -> metrics
                }
            case a: IQMCalibration =>
                a.edgeMetrics.map { case (edge, metrics) =>
                    val normalized = if (edge._1 <= edge._2) edge else edge.swap
                    normalized -> metrics
                }
            case a: RigettiCalibration =>
                a.edgeMetrics.map { case (edge, metrics) =>
                    val normalized = if (edge._1 <= edge._2) edge else edge.swap
                    normalized -> metrics
                }
            case _ =>
                Map.empty
        }

    case class CanonicalCalibration(
        eps1q: Map[(Int, String), Double],
        eps2q: Map[((Int, Int), String), Double],
        eps1qAvg: Option[Double],
        eps2qAvg: Option[Double],
        readoutFidelity: Map[Int, Double],
        readoutFidelityAvg: Option[Double],
        t1: Map[Int, Double],
        t2: Map[Int, Double],
        t1Avg: Option[Double],
        t2Avg: Option[Double],
        dur1qNs: Map[String, Long],
        dur2qNs: Map[String, Long],
        durMeasNs: Option[Long],
        dur1qAvgNs: Option[Long],
        dur2qAvgNs: Option[Long],
        initSurvivalPerQubit: Option[Double] = None,
        executionModel: ExecutionModel = ExecutionModel.ParallelByQubitOrEdge,
        warnings: Vector[String] = Vector.empty,
        // Set false only for error budgets explicitly stripped of relaxation/dephasing.
        gateErrorsIncludeDecoherence: Boolean = true,
        pasqalTiming: Option[PasqalTimingModel] = None,
        analyticalAssumptions: Vector[String] = Vector.empty,
        // Explicit model rate also represents calibrated zero decay without using infinite T1/T2.
        memoryInfidelityRatePerSecond: Option[Double] = None
    ){
        private def edgeKey(a: Int, b: Int): (Int, Int) = if (a <= b) (a, b) else (b, a)

        private def probability(value: Option[Double]): Option[Double] =
            value.filter(p => p.isFinite && p >= 0.0 && p <= 1.0)

        def epsOptionFor(op: Gate): Option[Double] = {
            val (name, qs) = gateKey(op)
            qs match {
                case _ if op.isInstanceOf[Measure] || name == "BARRIER" || name == "GPHASE" => Some(0.0)
                case Vector(q) => probability(eps1q.get((q, name))).orElse(probability(eps1qAvg))
                case Vector(a, b) => probability(eps2q.get((edgeKey(a, b), name))).orElse(probability(eps2qAvg))
                case _ => None // Higher-arity operations must be lowered first.
            }
        }

        def epsFor(op: Gate): Double = epsOptionFor(op).getOrElse(0.0)

        def durationNsOptionFor(op: Gate): Option[Long] = {
            val (name, qs) = gateKey(op)
            op match {
                case Measure(_) => durMeasNs.filter(_ >= 0L)
                case GPhase(_) => Some(0L)
                case _ if name == "BARRIER" => Some(0L)
                case _ if pasqalTiming.nonEmpty => qurator.util.PasqalAnalyticalEstimator.durationNs(op, pasqalTiming.get)
                case _ if qs.size == 1 => dur1qNs.get(name).filter(_ >= 0L).orElse(dur1qAvgNs.filter(_ >= 0L))
                case _ if qs.size == 2 => dur2qNs.get(name).filter(_ >= 0L).orElse(dur2qAvgNs.filter(_ >= 0L))
                case _ => None
            }
        }

        def durationNsFor(op: Gate): Long = durationNsOptionFor(op).getOrElse(0L).max(0L)

        def readoutFidFor(q: Int): Double =
            readoutFidelity.get(q).orElse(readoutFidelityAvg).getOrElse(0.0)

        private def coherence(value: Option[Double]): Option[Double] = value.filter(t => t.isFinite && t > 0.0)

        def t2For(q: Int): Option[Double] = coherence(t2.get(q)).orElse(coherence(t2Avg))
        def t1For(q: Int): Option[Double] = coherence(t1.get(q)).orElse(coherence(t1Avg))

    }

    def gateTimingKey(op: Gate): GateTimingKey = {
        val (name, _) = gateKey(op)
        val params = op match {
            case Phase(t, _) => Vector(t)
            case RX(t, _) => Vector(t)
            case RY(t, _) => Vector(t)
            case RZ(t, _) => Vector(t)
            case U(t, p, l, _) => Vector(t, p, l)
            case U2(p, l, _) => Vector(p, l)
            case U3(t, p, l, _) => Vector(t, p, l)
            case CP(_, t, _) => Vector(t)
            case CRX(_, t, _) => Vector(t)
            case CRY(_, t, _) => Vector(t)
            case CRZ(_, t, _) => Vector(t)
            case CU(_, t, p, l, _) => Vector(t, p, l)
            case NamedGate(_, ps, _) => ps
            case _ => Vector.empty
        }
        GateTimingKey(if (name == "CRotate") "CRZ" else name.toUpperCase, params.map(_.trim))
    }

    /** Shared gate identity for timing, scoring and dependency reconstruction. */
    def gateKey(op: Gate): (String, Vector[Int]) = op match {
        case X(q) => "X" -> Vector(q)
        case Y(q) => "Y" -> Vector(q)
        case Z(q) => "Z" -> Vector(q)
        case H(q) => "H" -> Vector(q)
        case S(q) => "S" -> Vector(q)
        case SDG(q) => "SDG" -> Vector(q)
        case T(q) => "T" -> Vector(q)
        case TDG(q) => "TDG" -> Vector(q)
        case SX(q) => "SX" -> Vector(q)
        case SXDG(q) => "SXDG" -> Vector(q)
        case Id(q) => "ID" -> Vector(q)
        case Phase(_, q) => "P" -> Vector(q)
        case RX(_, q) => "RX" -> Vector(q)
        case RY(_, q) => "RY" -> Vector(q)
        case RZ(_, q) => "RZ" -> Vector(q)
        case U(_, _, _, q) => "U" -> Vector(q)
        case U2(_, _, q) => "U2" -> Vector(q)
        case U3(_, _, _, q) => "U3" -> Vector(q)
        case CX(a, b) => "CX" -> Vector(a, b)
        case CY(a, b) => "CY" -> Vector(a, b)
        case CZ(a, b) => "CZ" -> Vector(a, b)
        case CH(a, b) => "CH" -> Vector(a, b)
        case Swap(a, b) => "SWAP" -> Vector(a, b)
        case CP(a, _, b) => "CP" -> Vector(a, b)
        case CRX(a, _, b) => "CRX" -> Vector(a, b)
        case CRY(a, _, b) => "CRY" -> Vector(a, b)
        case CRZ(a, _, b) => "CRotate" -> Vector(a, b)
        case CU(a, _, _, _, b) => "CU" -> Vector(a, b)
        case CCX(a, b, c) => "CCX" -> Vector(a, b, c)
        case Measure(q) => "MEASURE" -> Vector(q)
        case Reset(q) => "RESET" -> Vector(q)
        case GPhase(_) => "GPHASE" -> Vector.empty
        case NamedGate(name, _, qs) => name.trim.toUpperCase -> qs
    }

    case class FidelityEstimate(
        logPOps: Double,
        logPDecoh: Double,
        logPTotal: Double,
        pTotal: Double,
        perQubitEndTimeSec: Map[Int, Double],
        logPCrosstalk: Double = 0.0,
        logPTransport: Double = 0.0,
        logPInitialization: Double = 0.0,
        schedule: Option[HardwareSchedule] = None,
        analyticalCoverage: Option[AnalyticalFidelityCoverage] = None
    ) {
        def decoherence: Option[DecoherenceEstimate] = schedule.map(_.decoherence)
        def warnings: Vector[String] = schedule.toVector.flatMap(_.warnings)
    }

    /** Describes a partial analytical noise score, never a calibrated full-circuit fidelity. */
    case class AnalyticalFidelityCoverage(
        modeledContributions: Vector[String],
        unmodeledEffects: Vector[String],
        assumptions: Vector[String]
    ) {
        def isComplete: Boolean = unmodeledEffects.isEmpty
    }

    /** Coverage of the memory-noise calculation, not a guarantee of complete device calibration. */
    case class DecoherenceEstimate(
        logP: Double,
        perQubitExposureSec: Map[Int, Double],
        missingT1Qubits: Set[Int],
        missingT2Qubits: Set[Int],
        adjustedT2Qubits: Set[Int],
        missingDurationOperations: Vector[Int],
        uncalibratedGateOperations: Vector[Int],
        warnings: Vector[String]
    ) {
        def isComplete: Boolean = missingT1Qubits.isEmpty && missingT2Qubits.isEmpty &&
            adjustedT2Qubits.isEmpty && missingDurationOperations.isEmpty && uncalibratedGateOperations.isEmpty
    }

    case class TimedGate(
        index: Int,
        qubits: Vector[Int],
        startNs: Long,
        gateStartNs: Long,
        endNs: Long,
        resources: Vector[String],
        isMeasurement: Boolean
    )

    case class HardwareSchedule(
        operations: Vector[TimedGate],
        perQubitEndTimeSec: Map[Int, Double],
        makespanNs: Long,
        averageGateParallelism: Double,
        peakGateParallelism: Int,
        logPCrosstalk: Double,
        logPTransport: Double,
        warnings: Vector[String],
        decoherence: DecoherenceEstimate
    )

}
