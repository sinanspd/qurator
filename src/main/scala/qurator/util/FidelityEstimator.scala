package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import scala.math.{log, log1p, exp, max}

object FidelityEstimator{

    def normalizeCalibration(c: DeviceCalibration): CanonicalCalibration = c match {
        case a : IBMCalibration => ???
        case a : IonQCalibration => 
            val f1 = a.avg1qFidelityPct / 100.0
            val f2 = a.avg2qFidelityPct / 100.0
                 CanonicalCalibration(
                    eps1q = Map.empty,
                    eps2q = Map.empty,
                    eps1qAvg = Some(1.0 - f1),
                    eps2qAvg = Some(1.0 - f2),
                    readoutFidelity = Map.empty,
                    readoutFidelityAvg = Some(a.avgReadoutFidelity),
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
            CanonicalCalibration(
                eps1q = Map.empty,
                eps2q = Map.empty,
                eps1qAvg = Some(1.0 - a.oneQGateFidelity),
                eps2qAvg = Some(1.0 - a.twoQGateFidelity),
                readoutFidelity = Map.empty,
                readoutFidelityAvg = Some(a.readoutFidelity),
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
                case a @ (X(_) | H(_) | Measure(_) | Rotate(_, _) | RZ(_ ,_)) =>
                    val q = a match { // this is dumb but oh well
                        case X(q)       => q
                        case H(q)       => q
                        case Measure(q) => q
                        case Rotate(_, q) => q
                        case RZ(_, q) => q
                    }
                    val dur = cal.durationNsFor(a)
                    val start = availNs(q)
                    val end = start + dur
                    availNs.update(q, end)
                case op @ (CX(_, _) | CZ(_, _) | Swap(_, _) | CRotate(_, _, _)) =>
                    val (a, b) = op match{
                        case CX(a, b) => (a, b)
                        case CZ(a, b) => (a, b)
                        case Swap(a , b) => (a, b)
                        case CRotate(a, _, b) => (a, b)
                        case Rotate(a ,b) => (a, b)
                        case RZ(a, b) => (a, b)
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
                    case a @ (X(_) | H(_) | Rotate(_, _) | RZ(_ ,_))  =>
                        val (q, g) = a match { 
                            case X(q)       => (q, "X")
                            case H(q)       => (q, "H")
                            case Rotate(_, q) => (q, "Rotate")
                            case RZ(_, q) => (q, "RZ")
                        }
                        val eps = cal.epsFor(op)
                        acc + log1p(-eps)

                    case a @ (CX(_, _) | CZ(_, _) | Swap(_, _) | CRotate(_, _, _) ) =>
                        val (a, b, g) = op match{
                            case CX(a, b) => (a, b, "CX")
                            case CZ(a, b) => (a, b, "CZ")
                            case Swap(a , b) => (a, b, "SWAP")
                            case CRotate(a, _, b) => (a, b, "CRotate")
                            case Rotate(a ,b) => (a, b, "Rotate")
                            case RZ(a, b) => (a, b, "RZ")
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