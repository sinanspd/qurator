package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import scala.math.{log, log1p, exp, max}

object FidelityEstimator{

    def normalizeCalibration(c: DeviceCalibration): CanonicalCalibration = c match {
        case a : IBMCalibration =>
            val t1Map = a.t1Seconds.toMap
            val t2Map = a.t2Seconds.toMap

            val readoutFid =
                1.0 - (a.probMeasu0Presp1 + a.probMeasu1Presp0) / 2.0

            val readoutMap: Map[Int, Double] =
                a.qubits.map(q => q -> readoutFid).toMap

            val directedEdges: List[(Int, Int)] =
                a.edges.flatMap { case (q1, q2) => List((q1, q2), (q2, q1)) }.distinct

            val eps1qMap: Map[(Int, String), Double] =
                a.qubits.flatMap { q =>
                    List(
                        (q, "ID")     -> a.idError,
                        (q, "RX")     -> a.rxError,
                        (q, "X")      -> a.pauliXError,
                        (q, "RZ")     -> 0.0,              // virtual Z on IBM
                        (q, "H")      -> a.rxError,        // approximation
                        (q, "Rotate") -> a.rxError         // approximation
                    )
                }.toMap

            val eps2qMap: Map[((Int, Int), String), Double] =
                directedEdges.flatMap { case edge @ (q1, q2) =>
                    List(
                        (edge, "CZ")      -> a.czError,
                        (edge, "RZZ")     -> a.rzzError,
                        (edge, "CX")      -> a.czError,                // approximation
                        (edge, "CRotate") -> a.rzzError,               // approximation
                        (edge, "SWAP")    -> math.min(1.0, 3.0 * a.czError)
                    )
                }.toMap

            val oneQErrs = List(a.idError, a.rxError, a.pauliXError).filter(_ >= 0.0)
            val twoQErrs = List(a.czError, a.rzzError).filter(_ >= 0.0)

            val avg1qErr =
                if (oneQErrs.nonEmpty) Some(oneQErrs.sum / oneQErrs.size) else None

            val avg2qErr =
                if (twoQErrs.nonEmpty) Some(twoQErrs.sum / twoQErrs.size) else None

            val oneQDurs = List(a.idLengthNs, a.singleQGateLengthNs).filter(_ > 0L)
            val twoQDurs = List(a.czGateLengthNs, a.rzzGateLengthNs, a.twoQGateLengthNs).filter(_ > 0L)

            val avg1qDurNs =
                if (oneQDurs.nonEmpty) Some(oneQDurs.sum / oneQDurs.size) else None

            val avg2qDurNs =
                if (twoQDurs.nonEmpty) Some(twoQDurs.sum / twoQDurs.size) else None

            CanonicalCalibration(
                eps1q = eps1qMap,
                eps2q = eps2qMap,
                eps1qAvg = avg1qErr,
                eps2qAvg = avg2qErr,
                readoutFidelity = readoutMap,
                readoutFidelityAvg = Some(readoutFid),
                t1 = t1Map,
                t2 = t2Map,
                t1Avg = Some(a.t1AvgSeconds),
                t2Avg = Some(a.t2AvgSeconds),
                dur1qNs = Map(
                    "ID"     -> a.idLengthNs,
                    "RX"     -> a.singleQGateLengthNs,
                    "X"      -> a.singleQGateLengthNs,
                    "RZ"     -> 0L,
                    "H"      -> a.singleQGateLengthNs,
                    "Rotate" -> a.singleQGateLengthNs
                ),
                dur2qNs = Map(
                    "CZ"      -> a.czGateLengthNs,
                    "RZZ"     -> a.rzzGateLengthNs,
                    "CX"      -> a.twoQGateLengthNs,
                    "CRotate" -> a.rzzGateLengthNs,
                    "SWAP"    -> 3L * a.czGateLengthNs
                ),
                durMeasNs = Some(a.readoutLengthNs),
                dur1qAvgNs = avg1qDurNs,
                dur2qAvgNs = avg2qDurNs,
                initSurvivalPerQubit = None
            )
                case a : IonQCalibration => 
                    val f1 = a.avg1qFidelityPct / 100.0
                    val f2 = a.avg2qFidelityPct / 100.0
                        CanonicalCalibration(
                            eps1q = Map.empty,
                            eps2q = Map.empty,
                            eps1qAvg = Some(1.0 - f1),
                            eps2qAvg = Some(1.0 - f2),
                            readoutFidelity = Map.empty,
                            readoutFidelityAvg = Some(a.avgReadoutFidelity / 100.0),
                            t1 = Map.empty,
                            t2 = Map.empty,
                            t1Avg = Some(a.t1Seconds),
                            t2Avg = Some(a.t2Seconds),
                            dur1qNs = Map.empty,
                            dur2qNs = Map.empty,
                            durMeasNs = Some((a.readoutDurationSec * 1e9).toLong),
                            dur1qAvgNs = Some((a.oneQGateDurationSec * 1e9).toLong),
                            dur2qAvgNs = Some((a.twoQGateDurationSec * 1e9).toLong)
                        )
        case a : IQMCalibration => 
              val f1 = a.q1fidelity / 100.0
              val f2 = a.q2fidelity / 100.0
              CanonicalCalibration(
                eps1q = Map.empty,
                eps2q = Map.empty,
                eps1qAvg = Some(1.0 - f1),
                eps2qAvg = Some(1.0 - f2),

                readoutFidelity = Map.empty,
                readoutFidelityAvg = Some(a.readoutFidelity / 100),

                t1 = Map.empty,
                t2 = Map.empty,
                t1Avg = Some(a.t1),
                t2Avg = Some(a.t2),

                dur1qNs = Map.empty,
                dur2qNs = Map.empty,
                durMeasNs = None,
                dur1qAvgNs = None,
                dur2qAvgNs = None,

                initSurvivalPerQubit = None
            )
            // val fp = a.typicalDetectionFalsePositive
            // val fn = a.typicalDetectionFalseNegative
            // val readoutFid = 1.0 - (fp + fn) / 2.0
            // val vacancy = a.typicalVacancyError.orElse(a.typicalFillingError).getOrElse(Double.NaN)
            // val pOccupied = if (vacancy.isFinite) 1.0 - vacancy else Double.NaN
            // val loss = a.typicalAtomLossProbability.getOrElse(Double.NaN)
            // val pSurvive = if (loss.isFinite) 1.0 - loss else Double.NaN
            // val initSurvivalPerQubit =
            //     if (pOccupied.isFinite && pSurvive.isFinite) Some(pOccupied * pSurvive)
            //     else if (pOccupied.isFinite) Some(pOccupied)
            //     else if (pSurvive.isFinite) Some(pSurvive)
            //     else None
            // val t1Avg = a.t1SingleSec
            // val t2Avg = a.t2EchoSingleSec.orElse(a.t2SingleSec)
            // CanonicalCalibration(
            //     eps1q = Map.empty,
            //     eps2q = Map.empty,
            //     eps1qAvg = None,                
            //     eps2qAvg = None,
            //     readoutFidelity = Map.empty,
            //     readoutFidelityAvg = Some(readoutFid),
            //     t1 = Map.empty,
            //     t2 = Map.empty,
            //     t1Avg = t1Avg,
            //     t2Avg = t2Avg,
            //     dur1qNs = Map.empty,
            //     dur2qNs = Map.empty,
            //     durMeasNs = None,
            //     dur1qAvgNs = None,
            //     dur2qAvgNs = None,
            //     initSurvivalPerQubit = initSurvivalPerQubit
            // )

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
            val f1 = a.avg1qFidelityPct / 100.0
            val fro = a.readoutFidelityPct / 100.0
            val fswap = a.swapFidelityPct / 100.0

            CanonicalCalibration(
                eps1q = Map.empty,
                eps2q = Map.empty,
                eps1qAvg = Some(1.0 - f1),
                eps2qAvg = Some(1.0 - fswap),
                readoutFidelity = Map.empty,
                readoutFidelityAvg = Some(fro),
                t1 = Map.empty,
                t2 = Map.empty,
                t1Avg = Some(a.t1Seconds),
                t2Avg = Some(a.t2Seconds),
                dur1qNs = Map.empty,
                dur2qNs = Map("SWAP" -> a.swapGateDurationNs),
                durMeasNs = Some(a.readoutDurationNs),
                dur1qAvgNs = Some(a.oneQGateDurationNs),
                dur2qAvgNs = Some(a.twoQGateDurationNs)
            )

        case a : AQTCalibration => 
            val f1  = a.oneQGateFidelity / 100.0
            val f2  = a.twoQGateFidelity / 100.0
            val fro = a.readoutFidelity / 100.0

            CanonicalCalibration(
                eps1q = Map.empty,
                eps2q = Map.empty,
                eps1qAvg = Some(1.0 - f1),
                eps2qAvg = Some(1.0 - f2),
                readoutFidelity = Map.empty,
                readoutFidelityAvg = Some(fro),
                t1 = Map.empty,
                t2 = Map.empty,
                t1Avg = Some(a.t1Seconds),
                t2Avg = Some(a.t2Seconds),
                dur1qNs = Map.empty,
                dur2qNs = Map.empty,
                durMeasNs = Some((a.readoutDurationSec * 1e9).toLong),
                dur1qAvgNs = Some((a.oneQGateDurationSec * 1e9).toLong),
                dur2qAvgNs = Some((a.twoQGateDurationSec * 1e9).toLong)
            )
        }


        def scheduleEndTimesSec(ops: List[Gate], cal: CanonicalCalibration): Map[Int, Double] = {
            val availNs = scala.collection.mutable.Map.empty[Int, Long].withDefaultValue(0L)

            ops.foreach {
                case a @ (X(_) | H(_) | Measure(_) | RX(_, _) | RZ(_ ,_)) =>
                    val q = a match { // this is dumb but oh well
                        case X(q)       => q
                        case H(q)       => q
                        case Measure(q) => q
                        case RX(_, q) => q
                        case RZ(_, q) => q
                    }
                    val dur = cal.durationNsFor(a)
                    val start = availNs(q)
                    val end = start + dur
                    availNs.update(q, end)
                case op @ (CX(_, _) | CZ(_, _) | Swap(_, _) | CRZ(_, _, _)) =>
                    val (a, b) = op match{
                        case CX(a, b) => (a, b)
                        case CZ(a, b) => (a, b)
                        case Swap(a , b) => (a, b)
                        case CRZ(a, _, b) => (a, b)
                    }
                    val dur = cal.durationNsFor(op)
                    val start = Math.max(availNs(a), availNs(b))
                    val end = start + dur
                    availNs.update(a, end)
                    availNs.update(b, end)

            }

            availNs.toMap.view.mapValues(ns => ns.toDouble / 1e9).toMap
        }


        def score(compiled: Circuit, cal: CanonicalCalibration): FidelityEstimate = {
            val endTimesSec = scheduleEndTimesSec(compiled.remainingGates, cal)

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

            FidelityEstimate(
                logPOps = logPOps,
                logPDecoh = logPDecoh,
                logPTotal = logPTotal,
                pTotal = pTotal,
                perQubitEndTimeSec = endTimesSec
            )
        }
}