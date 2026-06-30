package qurator

import cats.effect.IO
import weaver.SimpleIOSuite
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.cutting._
import qurator.domain.device.Device
import qurator.util.CuttingStrategies
import qurator.util.HardwareAwareCuttingPlanner

object HardwareAwareCuttingPlannerSuite extends SimpleIOSuite {

    private val device =
        Device(
            platform = "IBM",
            platformId = "ibm_test",
            qubits = 8,
            queueLength = 0,
            t1 = 100f,
            t2 = 100f,
            gateSet = List(H(0), X(0), CX(0, 1))
        )

    private def lineCalibration(width: Int, highQualityRegion: Int): IBMCalibration = {
        val qubits = (0 until width).toList
        val edges = (0 until (width - 1)).toList.map(i => i -> (i + 1))
        val goodEdges = edges.take(math.max(0, highQualityRegion - 1)).toSet

        IBMCalibration(
            qubits = qubits,
            edges = edges,
            t1Seconds = qubits.map(_ -> 100.0),
            t2Seconds = qubits.map(_ -> 80.0),
            t1AvgSeconds = 100.0,
            t2AvgSeconds = 80.0,
            probMeasu0Presp1 = 0.005,
            probMeasu1Presp0 = 0.005,
            idError = 0.001,
            rxError = 0.001,
            pauliXError = 0.001,
            czError = 0.20,
            rzzError = 0.20,
            readoutLengthNs = 100L,
            singleQGateLengthNs = 10L,
            idLengthNs = 10L,
            twoQGateLengthNs = 100L,
            czGateLengthNs = 100L,
            rzzGateLengthNs = 100L,
            topology = Some(CalibrationTopology(qubits, edges)),
            qubitMetrics = qubits.map { q =>
                q -> QubitCalibrationMetrics(
                    readoutFidelity = Some(0.995),
                    gateErrors = Map("X" -> 0.001, "H" -> 0.001),
                    gateDurationsNs = Map("X" -> 10L, "H" -> 10L, "MEASURE" -> 100L)
                )
            }.toMap,
            edgeMetrics = edges.map { edge =>
                val error = if (goodEdges.contains(edge)) 0.005 else 0.20
                edge -> EdgeCalibrationMetrics(
                    gateErrors = Map("CX" -> error),
                    gateDurationsNs = Map("CX" -> 100L)
                )
            }.toMap
        )
    }

    private val chainCircuit =
        Circuit(
            remainingGates = List(
                H(0),
                CX(0, 1),
                CX(1, 2),
                CX(2, 3),
                CX(3, 4),
                CX(4, 5),
                CX(5, 6),
                CX(6, 7),
                Measure(0)
            ),
            qubits = 8,
            name = "chain"
        )

    test("hardware-aware planner infers effective width from calibrated connected regions") {
        val calibration = lineCalibration(width = 8, highQualityRegion = 4)
        val request =
            CuttingRequest(
                circuit = chainCircuit,
                devices = List(device),
                targetEstimatedFidelity = 0.90,
                shots = Some(1000L),
                paretoLimit = 4
            )

        HardwareAwareCuttingPlanner
            .plan[IO](
                request = request,
                fetchCalibration = _ => IO.pure(calibration),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = HardwareAwareCuttingPlanner.Config(effectiveWidthFidelityThreshold = 0.98)
            )
            .map { decision =>
                val selected = decision.selected
                val inferredWidth = selected.deviceWidths.head.effectiveWidth

                expect.all(
                    inferredWidth == 4,
                    selected.metrics.feasible,
                    selected.parameters.maxSubcircuitWidth <= 4,
                    selected.parameters.maxSubcircuits == selected.subcircuits.size,
                    selected.parameters.maxCuts == selected.cutLocations.size,
                    decision.frontier.nonEmpty,
                    decision.frontier.head == selected
                )
            }
    }

    test("small-circuit no-cut fast path avoids calibration fetch") {
        val request =
            CuttingRequest(
                circuit = Circuit(List(H(0), CX(0, 1), Measure(0)), 5, "small"),
                devices = List(device),
                targetEstimatedFidelity = 0.90,
                shots = Some(1000L)
            )

        HardwareAwareCuttingPlanner
            .plan[IO](
                request = request,
                fetchCalibration = _ => IO.raiseError(new RuntimeException("calibration should not be fetched")),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = HardwareAwareCuttingPlanner.Config(
                    smallCircuitNoCutFastPath = true,
                    cuttingMode = CuttingMode.SpatialWidth
                )
            )
            .map { decision =>
                expect.all(
                    decision.selected.name == "no-cut-small-circuit-fast-path",
                    decision.selected.subcircuits == List(request.circuit),
                    decision.selected.deviceWidths.isEmpty
                )
            }
    }

    test("temporal mode prefers idle-heavy wire cuts for small deep circuits") {
        val calibration =
            lineCalibration(width = 8, highQualityRegion = 8).copy(
                t1Seconds = (0 until 8).toList.map(_ -> 0.0000001),
                t2Seconds = (0 until 8).toList.map(_ -> 0.0000001),
                t1AvgSeconds = 0.0000001,
                t2AvgSeconds = 0.0000001
            )
        val deepCircuit =
            Circuit(
                remainingGates = H(0) :: List.fill(100)(H(1)) ::: List(CX(0, 1)),
                qubits = 2,
                name = "idle-heavy"
            )
        val request =
            CuttingRequest(
                circuit = deepCircuit,
                devices = List(device),
                targetEstimatedFidelity = 0.90,
                shots = Some(1000L),
                paretoLimit = 8
            )

        HardwareAwareCuttingPlanner
            .plan[IO](
                request = request,
                fetchCalibration = _ => IO.pure(calibration),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = HardwareAwareCuttingPlanner.Config(
                    cuttingMode = CuttingMode.TemporalDepth,
                    temporalDurationT1T2RatioThreshold = 0.10,
                    maxTemporalCutQubits = 1,
                    minTemporalBoundaryScore = 0.0,
                    smallCircuitNoCutFastPath = true
                )
            )
            .map { decision =>
                val temporal = decision.candidates.filter(_.name.startsWith("temporal-depth"))
                val candidate = temporal.headOption

                expect.all(
                    temporal.nonEmpty,
                    candidate.exists(_.cutLocations.nonEmpty),
                    candidate.exists(_.cutLocations.forall(_.gateName == "WIRE")),
                    candidate.exists(_.cutLocations.flatMap(_.qubits).distinct == List(0)),
                    candidate.exists(_.metrics.samplingOverhead >= 4.0),
                    candidate.exists(p => p.metrics.maxSubcircuitDurationNs < p.metrics.uncutDurationNs),
                    candidate.exists(_.metrics.durationReductionNs > 0L),
                    candidate.exists(_.parameters.maxSubcircuitWidth == request.circuit.qubits)
                )
            }
    }

    test("effective width inference is capped at circuit width when fast path is disabled") {
        val calibration = lineCalibration(width = 8, highQualityRegion = 8)
        val request =
            CuttingRequest(
                circuit = Circuit(
                    remainingGates = List(H(0), CX(0, 1), CX(1, 2), CX(2, 3), CX(3, 4), Measure(0)),
                    qubits = 5,
                    name = "bounded"
                ),
                devices = List(device),
                targetEstimatedFidelity = 0.90,
                shots = Some(1000L),
                paretoLimit = 4
            )

        HardwareAwareCuttingPlanner
            .plan[IO](
                request = request,
                fetchCalibration = _ => IO.pure(calibration),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = HardwareAwareCuttingPlanner.Config(
                    effectiveWidthFidelityThreshold = 0.98,
                    smallCircuitNoCutFastPath = false
                )
            )
            .map { decision =>
                val inferredWidth = decision.candidates.flatMap(_.deviceWidths).head.effectiveWidth

                expect.all(
                    inferredWidth == request.circuit.qubits,
                    decision.candidates.exists(_.parameters.maxSubcircuitWidth < request.circuit.qubits)
                )
            }
    }

    test("planner includes midpoint candidate with cut benefit diagnostics") {
        val calibration = lineCalibration(width = 8, highQualityRegion = 8)
        val request =
            CuttingRequest(
                circuit = Circuit(
                    remainingGates = List(
                        H(0),
                        H(3),
                        CX(1, 2),
                        Measure(0)
                    ),
                    qubits = 4,
                    name = "midpoint"
                ),
                devices = List(device),
                targetEstimatedFidelity = 0.90,
                shots = Some(1000L),
                paretoLimit = 6
            )

        HardwareAwareCuttingPlanner
            .plan[IO](
                request = request,
                fetchCalibration = _ => IO.pure(calibration),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = HardwareAwareCuttingPlanner.Config(
                    effectiveWidthFidelityThreshold = 0.98,
                    smallCircuitNoCutFastPath = false
                )
            )
            .map { decision =>
                val midpoint = decision.candidates.find(_.name == "midpoint-aggressive")

                expect.all(
                    midpoint.exists(_.parameters.maxSubcircuits == 2),
                    midpoint.exists(_.cutLocations.map(_.gateIndex) == List(2)),
                    midpoint.flatMap(_.metrics.apparentCutLogGain).isDefined,
                    midpoint.flatMap(_.metrics.fragmentOnlyLogGain).isDefined,
                    midpoint.flatMap(_.metrics.cutBenefitClassification).exists(_ != "no-cut-baseline")
                )
            }
    }

    test("planner includes surgical gate-cut candidates without forcing a full partition boundary") {
        val calibration = lineCalibration(width = 8, highQualityRegion = 8)
        val request =
            CuttingRequest(
                circuit = Circuit(
                    remainingGates = List(
                        H(0),
                        CX(0, 1),
                        H(2),
                        CX(1, 2),
                        Measure(0)
                    ),
                    qubits = 3,
                    name = "surgical"
                ),
                devices = List(device),
                targetEstimatedFidelity = 0.90,
                shots = Some(1000L),
                paretoLimit = 8
            )

        HardwareAwareCuttingPlanner
            .plan[IO](
                request = request,
                fetchCalibration = _ => IO.pure(calibration),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = HardwareAwareCuttingPlanner.Config(
                    cuttingMode = CuttingMode.SpatialWidth,
                    effectiveWidthFidelityThreshold = 0.98,
                    smallCircuitNoCutFastPath = false,
                    surgicalGateCutsEnabled = true,
                    maxSurgicalGateCuts = 1,
                    maxSurgicalGateCutCandidates = 4
                )
            )
            .map { decision =>
                val surgical = decision.candidates.find(_.name.startsWith("surgical-gate-"))

                expect.all(
                    surgical.exists(_.cutLocations.size == 1),
                    surgical.exists(_.cutLocations.head.gateName == "CX"),
                    surgical.exists(p => p.metrics.samplingOverhead >= 9.0 && p.metrics.samplingOverhead < 9.1),
                    surgical.exists(_.subcircuits.map(_.remainingGates.size).sum < request.circuit.remainingGates.size),
                    surgical.flatMap(_.metrics.apparentCutLogGain).isDefined,
                    surgical.flatMap(_.metrics.fragmentOnlyLogGain).isDefined
                )
            }
    }

    test("legacy subcircuit splitters can be adapted to the plan interface") {
        val strategy =
            CuttingStrategies.fromSubcircuits[IO]("manual") { (circuit, _) =>
                IO.pure(
                    List(
                        Circuit(List(H(0)), 1, s"${circuit.name}_left"),
                        Circuit(List(X(0)), 1, s"${circuit.name}_right")
                    )
                )
            }

        strategy(
            CuttingRequest(
                circuit = Circuit(List(H(0), X(1)), 2, "manual"),
                devices = List(device),
                targetEstimatedFidelity = 0.9
            )
        ).map { decision =>
            expect.all(
                decision.selected.name == "manual",
                decision.selected.subcircuits.size == 2,
                decision.selected.parameters.maxSubcircuits == 2,
                decision.selected.parameters.maxSubcircuitWidth == 1
            )
        }
    }
}
