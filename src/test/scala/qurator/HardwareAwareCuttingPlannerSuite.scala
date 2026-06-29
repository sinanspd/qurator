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
