package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import scala.math.{log, log1p, exp, max}

object FidelityEstimator{

    private def avgOption(values: Iterable[Double]): Option[Double] = {
        val xs = values.iterator.filter(_.isFinite).toVector
        if (xs.nonEmpty) Some(xs.sum / xs.size.toDouble) else None
    }

    private def pctToProb(value: Double): Option[Double] =
        Option.when(value.isFinite && value > 0.0)(if (value <= 1.0) value else value / 100.0)

    private def secondsOpt(value: Double): Option[Double] =
        Option.when(value.isFinite && value > 0.0)(value)

    private def nanosOpt(seconds: Double): Option[Long] =
        Option.when(seconds.isFinite && seconds > 0.0)((seconds * 1e9).toLong)

    private def avgLongOption(values: Iterable[Long]): Option[Long] = {
        val xs = values.iterator.filter(_ > 0L).toVector
        if (xs.nonEmpty) Some((xs.sum.toDouble / xs.size.toDouble).toLong) else None
    }

    private def readoutFidelity(metrics: QubitCalibrationMetrics): Option[Double] =
        metrics.readoutFidelity
            .orElse {
                for {
                    e01 <- metrics.probMeasu0Prep1
                    e10 <- metrics.probMeasu1Prep0
                } yield 1.0 - ((e01 + e10) / 2.0)
            }
            .orElse(metrics.readoutError.map(e => 1.0 - e))

    private def upper(name: String): String =
        name.trim.toUpperCase

    private def edgeKey(a: Int, b: Int): (Int, Int) =
        if (a <= b) (a, b) else (b, a)

    private def metricError(metrics: QubitCalibrationMetrics, names: String*): Option[Double] =
        names.iterator.map(upper).flatMap(metrics.gateErrors.get).toSeq.headOption

    private def metricDuration(metrics: QubitCalibrationMetrics, names: String*): Option[Long] =
        names.iterator.map(upper).flatMap(metrics.gateDurationsNs.get).find(_ > 0L)

    private def edgeMetricError(metrics: EdgeCalibrationMetrics, names: String*): Option[Double] =
        names.iterator.map(upper).flatMap(metrics.gateErrors.get).toSeq.headOption

    private def edgeMetricDuration(metrics: EdgeCalibrationMetrics, names: String*): Option[Long] =
        names.iterator.map(upper).flatMap(metrics.gateDurationsNs.get).find(_ > 0L)

    private def buildStandardized1qErrors(
        qubitMetrics: Map[Int, QubitCalibrationMetrics]
    ): Map[(Int, String), Double] =
        qubitMetrics.toList.flatMap { case (q, metrics) =>
            metrics.oneQubitFidelity.map(f => (q, "X") -> (1.0 - f)).toList
        }.toMap

    private def buildStandardized2qErrors(
        edgeMetrics: Map[(Int, Int), EdgeCalibrationMetrics]
    ): Map[((Int, Int), String), Double] =
        edgeMetrics.toList.flatMap { case (edge, metrics) =>
            metrics.gateFidelities.toList.map { case (gate, fidelity) =>
                (edgeKey(edge._1, edge._2), upper(gate)) -> (1.0 - fidelity)
            }
        }.toMap

    def normalizeCalibration(c: DeviceCalibration): CanonicalCalibration = c match {
        case a : IBMCalibration =>
            val qubitMetrics = a.qubitMetrics
            val edgeMetrics = a.edgeMetrics.map { case (edge, metrics) => edgeKey(edge._1, edge._2) -> metrics }
            val topology = topologyOf(a)
            val qubits = topology.map(_.qubits).getOrElse(a.qubits)
            val edges = topology.map(_.normalizedEdges).getOrElse(a.edges.map { case (x, y) => edgeKey(x, y) }.distinct)

            val t1Map =
                if (qubitMetrics.nonEmpty) qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t1Seconds.map(q -> _) }.toMap
                else a.t1Seconds.toMap

            val t2Map =
                if (qubitMetrics.nonEmpty) qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t2Seconds.map(q -> _) }.toMap
                else a.t2Seconds.toMap

            val fallbackReadoutFid =
                1.0 - ((a.probMeasu0Presp1 + a.probMeasu1Presp0) / 2.0)

            val readoutMap =
                if (qubitMetrics.nonEmpty) {
                    qubitMetrics.toList.flatMap { case (q, metrics) => readoutFidelity(metrics).map(q -> _) }.toMap
                } else qubits.map(q => q -> fallbackReadoutFid).toMap

            val eps1qMap =
                qubits.flatMap { q =>
                    val metrics = qubitMetrics.getOrElse(q, QubitCalibrationMetrics())
                    List(
                        (q, "ID")     -> metricError(metrics, "ID").getOrElse(a.idError),
                        (q, "RX")     -> metricError(metrics, "RX", "SX", "X").getOrElse(a.rxError),
                        (q, "SX")     -> metricError(metrics, "SX", "RX", "X").getOrElse(a.rxError),
                        (q, "X")      -> metricError(metrics, "X", "SX", "RX").getOrElse(a.pauliXError),
                        (q, "RZ")     -> metricError(metrics, "RZ").getOrElse(0.0),
                        (q, "H")      -> metricError(metrics, "H", "SX", "RX", "X").getOrElse(a.rxError),
                        (q, "Rotate") -> metricError(metrics, "RX", "SX", "X").getOrElse(a.rxError)
                    )
                }.toMap

            val eps2qMap =
                edges.flatMap { edge =>
                    val metrics = edgeMetrics.getOrElse(edge, EdgeCalibrationMetrics())
                    val cxErr = edgeMetricError(metrics, "CX", "ECR", "CZ").getOrElse(a.czError)
                    val czErr = edgeMetricError(metrics, "CZ", "ECR", "CX").getOrElse(a.czError)
                    val rzzErr = edgeMetricError(metrics, "RZZ").getOrElse(a.rzzError)
                    val base = List(
                        (edge, "CZ")      -> czErr,
                        (edge, "RZZ")     -> rzzErr,
                        (edge, "CX")      -> cxErr,
                        (edge, "CRotate") -> rzzErr,
                        (edge, "SWAP")    -> math.min(1.0, 3.0 * cxErr)
                    )
                    val maybeEcr = edgeMetricError(metrics, "ECR").map(err => (edge, "ECR") -> err).toList
                    base ++ maybeEcr
                }.toMap

            val measured1qErrors =
                if (qubitMetrics.nonEmpty) qubitMetrics.values.flatMap(_.gateErrors.values)
                else List(a.idError, a.rxError, a.pauliXError).filter(_ >= 0.0)

            val measured2qErrors =
                if (edgeMetrics.nonEmpty) edgeMetrics.values.flatMap(_.gateErrors.values)
                else List(a.czError, a.rzzError).filter(_ >= 0.0)

            val dur1qNs = Map(
                "ID"     -> avgLongOption(qubits.flatMap(q => metricDuration(qubitMetrics.getOrElse(q, QubitCalibrationMetrics()), "ID"))).getOrElse(a.idLengthNs),
                "RX"     -> avgLongOption(qubits.flatMap(q => metricDuration(qubitMetrics.getOrElse(q, QubitCalibrationMetrics()), "RX", "SX", "X"))).getOrElse(a.singleQGateLengthNs),
                "SX"     -> avgLongOption(qubits.flatMap(q => metricDuration(qubitMetrics.getOrElse(q, QubitCalibrationMetrics()), "SX", "RX", "X"))).getOrElse(a.singleQGateLengthNs),
                "X"      -> avgLongOption(qubits.flatMap(q => metricDuration(qubitMetrics.getOrElse(q, QubitCalibrationMetrics()), "X", "SX", "RX"))).getOrElse(a.singleQGateLengthNs),
                "RZ"     -> 0L,
                "H"      -> avgLongOption(qubits.flatMap(q => metricDuration(qubitMetrics.getOrElse(q, QubitCalibrationMetrics()), "H", "SX", "RX", "X"))).getOrElse(a.singleQGateLengthNs),
                "Rotate" -> avgLongOption(qubits.flatMap(q => metricDuration(qubitMetrics.getOrElse(q, QubitCalibrationMetrics()), "RX", "SX", "X"))).getOrElse(a.singleQGateLengthNs)
            )

            val cxDur = avgLongOption(edges.flatMap(e => edgeMetricDuration(edgeMetrics.getOrElse(e, EdgeCalibrationMetrics()), "CX", "ECR", "CZ"))).getOrElse(a.twoQGateLengthNs)
            val czDur = avgLongOption(edges.flatMap(e => edgeMetricDuration(edgeMetrics.getOrElse(e, EdgeCalibrationMetrics()), "CZ", "ECR", "CX"))).getOrElse(a.czGateLengthNs)
            val rzzDur = avgLongOption(edges.flatMap(e => edgeMetricDuration(edgeMetrics.getOrElse(e, EdgeCalibrationMetrics()), "RZZ"))).getOrElse(a.rzzGateLengthNs)
            val ecrDur = avgLongOption(edges.flatMap(e => edgeMetricDuration(edgeMetrics.getOrElse(e, EdgeCalibrationMetrics()), "ECR")))

            val dur2qNs =
                Map(
                    "CZ"      -> czDur,
                    "RZZ"     -> rzzDur,
                    "CX"      -> cxDur,
                    "CRotate" -> rzzDur,
                    "SWAP"    -> 3L * cxDur
                ) ++ ecrDur.map("ECR" -> _)

            CanonicalCalibration(
                eps1q = eps1qMap,
                eps2q = eps2qMap,
                eps1qAvg = avgOption(measured1qErrors),
                eps2qAvg = avgOption(measured2qErrors),
                readoutFidelity = readoutMap,
                readoutFidelityAvg = avgOption(readoutMap.values).orElse(pctToProb(fallbackReadoutFid * 100.0)),
                t1 = t1Map,
                t2 = t2Map,
                t1Avg = avgOption(t1Map.values).orElse(secondsOpt(a.t1AvgSeconds)),
                t2Avg = avgOption(t2Map.values).orElse(secondsOpt(a.t2AvgSeconds)),
                dur1qNs = dur1qNs,
                dur2qNs = dur2qNs,
                durMeasNs = avgLongOption(qubits.flatMap(q => metricDuration(qubitMetrics.getOrElse(q, QubitCalibrationMetrics()), "MEASURE"))).orElse(Option.when(a.readoutLengthNs > 0L)(a.readoutLengthNs)),
                dur1qAvgNs = avgLongOption(dur1qNs.values),
                dur2qAvgNs = avgLongOption(dur2qNs.values),
                initSurvivalPerQubit = None
            )

        case a : IonQCalibration =>
            CanonicalCalibration(
                eps1q = Map.empty,
                eps2q = Map.empty,
                eps1qAvg = pctToProb(a.avg1qFidelityPct).map(1.0 - _),
                eps2qAvg = pctToProb(a.avg2qFidelityPct).map(1.0 - _),
                readoutFidelity = a.qubitMetrics.toList.flatMap { case (q, metrics) => readoutFidelity(metrics).map(q -> _) }.toMap,
                readoutFidelityAvg = pctToProb(a.avgReadoutFidelity).orElse(avgOption(a.qubitMetrics.values.flatMap(readoutFidelity))),
                t1 = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t1Seconds.map(q -> _) }.toMap,
                t2 = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t2Seconds.map(q -> _) }.toMap,
                t1Avg = secondsOpt(a.t1Seconds).orElse(avgOption(a.qubitMetrics.values.flatMap(_.t1Seconds))),
                t2Avg = secondsOpt(a.t2Seconds).orElse(avgOption(a.qubitMetrics.values.flatMap(_.t2Seconds))),
                dur1qNs = Map.empty,
                dur2qNs = Map.empty,
                durMeasNs = nanosOpt(a.readoutDurationSec),
                dur1qAvgNs = nanosOpt(a.oneQGateDurationSec),
                dur2qAvgNs = nanosOpt(a.twoQGateDurationSec),
                executionModel = ExecutionModel.GlobalSerialGates
            )

        case a : IQMCalibration =>
            val eps1q = buildStandardized1qErrors(a.qubitMetrics)
            val eps2q = buildStandardized2qErrors(a.edgeMetrics.map { case (edge, metrics) => edgeKey(edge._1, edge._2) -> metrics })
            val readoutMap = a.qubitMetrics.toList.flatMap { case (q, metrics) => readoutFidelity(metrics).map(q -> _) }.toMap
            val t1Map = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t1Seconds.map(q -> _) }.toMap
            val t2Map = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t2Seconds.map(q -> _) }.toMap

            CanonicalCalibration(
                eps1q = eps1q,
                eps2q = eps2q,
                eps1qAvg = avgOption(eps1q.values).orElse(pctToProb(a.q1fidelity).map(1.0 - _)),
                eps2qAvg = avgOption(eps2q.values).orElse(pctToProb(a.q2fidelity).map(1.0 - _)),
                readoutFidelity = readoutMap,
                readoutFidelityAvg = avgOption(readoutMap.values).orElse(pctToProb(a.readoutFidelity)),
                t1 = t1Map,
                t2 = t2Map,
                t1Avg = avgOption(t1Map.values).orElse(secondsOpt(a.t1)),
                t2Avg = avgOption(t2Map.values).orElse(secondsOpt(a.t2)),
                dur1qNs = Map.empty,
                dur2qNs = Map.empty,
                durMeasNs = None,
                dur1qAvgNs = None,
                dur2qAvgNs = None,
                initSurvivalPerQubit = None
            )

        case a : QuEraCalibration =>
            val fp = a.typicalDetectionFalsePositive
            val fn = a.typicalDetectionFalseNegative
            val readoutFid = 1.0 - (fp + fn) / 2.0
            val vacancy = a.typicalVacancyError.orElse(a.typicalFillingError).getOrElse(Double.NaN)
            val pOccupied = if (vacancy.isFinite) 1.0 - vacancy else Double.NaN
            val loss = a.typicalAtomLossProbability.getOrElse(Double.NaN)
            val pSurvive = if (loss.isFinite) 1.0 - loss else Double.NaN
            val initSurvivalPerQubit =
                if (pOccupied.isFinite && pSurvive.isFinite) Some(pOccupied * pSurvive)
                else if (pOccupied.isFinite) Some(pOccupied)
                else if (pSurvive.isFinite) Some(pSurvive)
                else None
            val t1Avg = a.t1SingleSec
            val t2Avg = a.t2EchoSingleSec.orElse(a.t2SingleSec)
            CanonicalCalibration(
                eps1q = Map.empty,
                eps2q = Map.empty,
                eps1qAvg = None,
                eps2qAvg = None,
                readoutFidelity = Map.empty,
                readoutFidelityAvg = Some(readoutFid),
                t1 = Map.empty,
                t2 = Map.empty,
                t1Avg = t1Avg,
                t2Avg = t2Avg,
                dur1qNs = Map.empty,
                dur2qNs = Map.empty,
                durMeasNs = None,
                dur1qAvgNs = None,
                dur2qAvgNs = None,
                initSurvivalPerQubit = initSurvivalPerQubit
            )

        case a : RigettiCalibration =>
            val eps1q = buildStandardized1qErrors(a.qubitMetrics)
            val eps2q = buildStandardized2qErrors(a.edgeMetrics.map { case (edge, metrics) => edgeKey(edge._1, edge._2) -> metrics })
            val readoutMap = a.qubitMetrics.toList.flatMap { case (q, metrics) => readoutFidelity(metrics).map(q -> _) }.toMap
            val t1Map = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t1Seconds.map(q -> _) }.toMap
            val t2Map = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t2Seconds.map(q -> _) }.toMap

            CanonicalCalibration(
                eps1q = eps1q,
                eps2q = eps2q,
                eps1qAvg = avgOption(eps1q.values).orElse(pctToProb(a.avg1qFidelityPct).map(1.0 - _)),
                eps2qAvg = avgOption(eps2q.values).orElse(pctToProb(a.swapFidelityPct).map(1.0 - _)),
                readoutFidelity = readoutMap,
                readoutFidelityAvg = avgOption(readoutMap.values).orElse(pctToProb(a.readoutFidelityPct)),
                t1 = t1Map,
                t2 = t2Map,
                t1Avg = avgOption(t1Map.values).orElse(secondsOpt(a.t1Seconds)),
                t2Avg = avgOption(t2Map.values).orElse(secondsOpt(a.t2Seconds)),
                dur1qNs = Map.empty,
                dur2qNs = Option.when(a.swapGateDurationNs > 0L)(Map("SWAP" -> a.swapGateDurationNs)).getOrElse(Map.empty),
                durMeasNs = Option.when(a.readoutDurationNs > 0L)(a.readoutDurationNs),
                dur1qAvgNs = Option.when(a.oneQGateDurationNs > 0L)(a.oneQGateDurationNs),
                dur2qAvgNs = Option.when(a.twoQGateDurationNs > 0L)(a.twoQGateDurationNs)
            )

        case a : AQTCalibration =>
            val eps1q = buildStandardized1qErrors(a.qubitMetrics)
            val readoutMap = a.qubitMetrics.toList.flatMap { case (q, metrics) => readoutFidelity(metrics).map(q -> _) }.toMap
            val t1Map = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t1Seconds.map(q -> _) }.toMap
            val t2Map = a.qubitMetrics.toList.flatMap { case (q, metrics) => metrics.t2Seconds.map(q -> _) }.toMap

            CanonicalCalibration(
                eps1q = eps1q,
                eps2q = Map.empty,
                eps1qAvg = avgOption(eps1q.values).orElse(pctToProb(a.oneQGateFidelity).map(1.0 - _)),
                eps2qAvg = pctToProb(a.twoQGateFidelity).map(1.0 - _),
                readoutFidelity = readoutMap,
                readoutFidelityAvg = avgOption(readoutMap.values).orElse(pctToProb(a.readoutFidelity)),
                t1 = t1Map,
                t2 = t2Map,
                t1Avg = avgOption(t1Map.values).orElse(secondsOpt(a.t1Seconds)),
                t2Avg = avgOption(t2Map.values).orElse(secondsOpt(a.t2Seconds)),
                dur1qNs = Map.empty,
                dur2qNs = Map.empty,
                durMeasNs = nanosOpt(a.readoutDurationSec),
                dur1qAvgNs = nanosOpt(a.oneQGateDurationSec),
                dur2qAvgNs = nanosOpt(a.twoQGateDurationSec),
                executionModel = ExecutionModel.GlobalSerialGates
            )
        }

        private final case class ScheduleState(
            availableByQubitNs: Map[Int, Long],
            globalGateAvailableNs: Long
        ) {
            private def qubitAvailableNs(qubits: Vector[Int]): Long =
                qubits.map(q => availableByQubitNs.getOrElse(q, 0L)).foldLeft(0L)(math.max)

            def schedule(
                qubits: Vector[Int],
                durationNs: Long,
                usesGlobalGateResource: Boolean,
                executionModel: ExecutionModel
            ): ScheduleState = {
                val localStart = qubitAvailableNs(qubits)
                val globalStart =
                    executionModel match {
                        case ExecutionModel.GlobalSerialGates => globalGateAvailableNs
                        case ExecutionModel.ParallelByQubitOrEdge => 0L
                    }
                val start = math.max(localStart, globalStart)
                val end = start + durationNs
                val updatedAvailable =
                    qubits.foldLeft(availableByQubitNs) { case (acc, q) => acc.updated(q, end) }
                val updatedGlobal =
                    if (usesGlobalGateResource && executionModel == ExecutionModel.GlobalSerialGates) end
                    else globalGateAvailableNs

                ScheduleState(updatedAvailable, updatedGlobal)
            }
        }

        private object ScheduleState {
            val empty: ScheduleState =
                ScheduleState(Map.empty, 0L)
        }

        private final case class ScheduledOperation(
            qubits: Vector[Int],
            durationNs: Long,
            usesGlobalGateResource: Boolean
        )

        private def scheduledOperation(op: Gate, cal: CanonicalCalibration): Option[ScheduledOperation] =
            op match {
                case gate @ X(q) =>
                    Some(ScheduledOperation(Vector(q), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ H(q) =>
                    Some(ScheduledOperation(Vector(q), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ SX(q) =>
                    Some(ScheduledOperation(Vector(q), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ RX(_, q) =>
                    Some(ScheduledOperation(Vector(q), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ RZ(_, q) =>
                    Some(ScheduledOperation(Vector(q), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ RY(_, q) =>
                    Some(ScheduledOperation(Vector(q), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ Measure(q) =>
                    Some(ScheduledOperation(Vector(q), cal.durationNsFor(gate), usesGlobalGateResource = false))
                case gate @ CX(a, b) =>
                    Some(ScheduledOperation(Vector(a, b), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ CZ(a, b) =>
                    Some(ScheduledOperation(Vector(a, b), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ Swap(a, b) =>
                    Some(ScheduledOperation(Vector(a, b), cal.durationNsFor(gate), usesGlobalGateResource = true))
                case gate @ CRZ(a, _, b) =>
                    Some(ScheduledOperation(Vector(a, b), cal.durationNsFor(gate), usesGlobalGateResource = true))

                case _ =>
                    None
            }

        def scheduleEndTimesSec(ops: List[Gate], cal: CanonicalCalibration): Map[Int, Double] = {
            val finalState =
                ops.foldLeft(ScheduleState.empty) { (state, op) =>
                    scheduledOperation(op, cal).fold(state) { scheduled =>
                        state.schedule(
                            qubits = scheduled.qubits,
                            durationNs = scheduled.durationNs,
                            usesGlobalGateResource = scheduled.usesGlobalGateResource,
                            executionModel = cal.executionModel
                        )
                    }
                }

            finalState.availableByQubitNs.view.mapValues(ns => ns.toDouble / 1e9).toMap
        }


        def score(compiled: Circuit, cal: CanonicalCalibration): FidelityEstimate = {
            val endTimesSec = scheduleEndTimesSec(compiled.remainingGates, cal)

            val mqubit = compiled.remainingGates.map{
                case a @ (X(_) | H(_) | Measure(_) | RX(_, _) | RZ(_ ,_) | SX(_) | RY(_, _)) =>
                    val q = a match { // this is dumb but oh well
                        case X(q)       => q
                        case H(q)       => q
                        case SX(q) => q
                        case Measure(q) => q
                        case RX(_, q) => q
                        case RZ(_, q) => q
                        case RY(_, q) => q
                    }
                    q
                case op @ (CX(_, _) | CZ(_, _) | Swap(_, _) | CRZ(_, _, _)) =>
                    val (a, b) = op match{
                        case CX(a, b) => (a, b)
                        case CZ(a, b) => (a, b)
                        case Swap(a , b) => (a, b)
                        case CRZ(a, _, b) => (a, b)
                    }
                    Math.max(a, b)
            }.max

            val logPOps = compiled.remainingGates.foldLeft(0.0) { (acc, op) =>
                op match {
                    case a @ (X(_) | H(_) | RX(_, _) | RZ(_ ,_))  =>
                        val (q, g) = a match { 
                            case X(q)       => (q, "X")
                            case H(q)       => (q, "H")
                            case RX(_, q) => (q, "Rotate")
                            case RZ(_, q) => (q, "RZ")
                        }
                        val eps = cal.epsFor(op)
                        acc + log1p(-eps)

                    case a @ (CX(_, _) | CZ(_, _) | Swap(_, _) | CRZ(_, _, _) ) =>
                        val (a, b, g) = op match{
                            case CX(a, b) => (a, b, "CX")
                            case CZ(a, b) => (a, b, "CZ")
                            case Swap(a , b) => (a, b, "SWAP")
                            case CRZ(a, _, b) => (a, b, "CRotate")
                        }
                        val eps = cal.epsFor(op)
                        acc + log1p(-eps)

                    case a @ Measure(q) =>
                        val fro = cal.readoutFidFor(q)
                        acc + log(fro)

                    case _ =>
                        acc
                }
            }

            val logPDecoh = endTimesSec.foldLeft(0.0) { case (acc, (q, tSec)) =>
                cal.t2For(q) match {
                    case Some(t2Sec) if t2Sec > 0.0 =>
                    acc + (-tSec / t2Sec)
                    case _ =>
                    acc
                }
            }

            val logPTotal = logPOps + logPDecoh
            val pTotal = Math.exp(logPTotal) 
            val est = FidelityEstimate(
                logPOps = logPOps,
                logPDecoh = logPDecoh,
                logPTotal = logPTotal,
                pTotal = pTotal,
                perQubitEndTimeSec = endTimesSec
            )

            val fcoeff =
                if (mqubit > 20) 0.3
                else if (mqubit > 10) 0.7
                else 1.0

            val adjusted = est.copy(
                pTotal = est.pTotal * fcoeff,
                logPTotal = est.logPTotal + math.log(fcoeff)
            )

            adjusted
        }
}
