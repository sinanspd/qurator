package qurator.domain

import qurator.domain.circuit._

object calibration {

    sealed trait DeviceCalibration 

    sealed trait ExecutionModel
    object ExecutionModel {
        case object ParallelByQubitOrEdge extends ExecutionModel
        case object GlobalSerialGates extends ExecutionModel
    }

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
        updatedAt: Option[String] = None
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
        updatedAt: Option[String] = None
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
        updatedAt: Option[String] = None
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
        executionModel: ExecutionModel = ExecutionModel.ParallelByQubitOrEdge
    ){
        private def edgeKey(a: Int, b: Int): (Int, Int) = if (a <= b) (a, b) else (b, a)

        def epsFor(op: Gate): Double =
            op match {
                case a @ (X(_) | H(_) | RX(_, _) | RZ(_, _))  =>
                    val (q, g) = a match { // this is dumb but oh well
                        case X(q)       => (q, "X")
                        case H(q)       => (q, "H")
                        case RX(_, q)   => (q, "RX")
                        case RZ(_, q)   => (q, "RZ")
                    }
                    eps1q.get((q, g))
                    .orElse(eps1qAvg)
                    .getOrElse(0.0) //TODO: Should we have a better default value here? 

                case a @ (CX(_, _) | CZ(_, _) | Swap(_, _) | CRZ(_, _, _)) =>
                    val (a, b, g) = op match{
                        case CX(a, b) => (a, b, "CX")
                        case CZ(a, b) => (a, b, "CZ")
                        case Swap(a , b) => (a, b, "SWAP")
                        case CRZ(a, _, b) => (a, b, "CRotate")
                    }
                    eps2q.get((edgeKey(a, b), g))
                    .orElse(eps2qAvg)
                    .getOrElse(0.0)

                case Measure(_) =>
                    0.0
                case _ =>
                    0.0
            }

            def durationNsFor(op: Gate): Long =
            op match {
                case a : X =>
                    dur1qNs.get("X").orElse(dur1qAvgNs).getOrElse(0L)

                case a : H =>
                    dur1qNs.get("H").orElse(dur1qAvgNs).getOrElse(0L)

                case a : CX =>
                     dur2qNs.get("CX").orElse(dur2qAvgNs).getOrElse(0L)

                case a : CZ =>
                     dur2qNs.get("CZ").orElse(dur2qAvgNs).getOrElse(0L)

                case a : Swap =>
                     dur2qNs.get("SWAP").orElse(dur2qAvgNs).getOrElse(0L)

                case a : CRZ =>
                     dur2qNs.get("CRotate").orElse(dur2qAvgNs).getOrElse(0L)

                case a : RX =>
                     dur2qNs.get("Rotate").orElse(dur1qAvgNs).getOrElse(0L)

                case a : RZ =>
                     dur2qNs.get("RZ").orElse(dur1qAvgNs).getOrElse(0L)

                case a : Measure =>
                    durMeasNs.getOrElse(0L)
                case _ =>
                    0L
            }

            def readoutFidFor(q: Int): Double =
                readoutFidelity.get(q).orElse(readoutFidelityAvg).getOrElse(0.0)

            def t2For(q: Int): Option[Double] = t2.get(q).orElse(t2Avg)
            def t1For(q: Int): Option[Double] = t1.get(q).orElse(t1Avg)

    }

    case class FidelityEstimate(
        logPOps: Double,
        logPDecoh: Double,
        logPTotal: Double,
        pTotal: Double,
        perQubitEndTimeSec: Map[Int, Double]
    ) 

}
