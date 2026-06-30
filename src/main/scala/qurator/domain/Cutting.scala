package qurator.domain

import qurator.domain.circuit.Circuit
import qurator.domain.device.Device

object cutting {

    final case class CuttingObjectiveWeights(
        samplingOverhead: Double = 0.25,
        classicalReconstruction: Double = 0.10,
        hardwareError: Double = 0.35,
        routingSwap: Double = 0.20,
        queueRun: Double = 0.10
    )

    final case class CuttingBudgets(
        maxSamplingOverhead: Option[Double] = None,
        maxClassicalMemoryBytes: Option[Double] = None,
        maxShots: Option[Long] = None,
        maxQueueRunMillis: Option[Long] = None
    )

    final case class CuttingRequest(
        circuit: Circuit,
        devices: List[Device],
        targetEstimatedFidelity: Double,
        shots: Option[Long] = None,
        objectiveWeights: CuttingObjectiveWeights = CuttingObjectiveWeights(),
        budgets: CuttingBudgets = CuttingBudgets(),
        paretoLimit: Int = 5,
        effectiveWidthEnabled: Boolean = true
    )

    final case class CuttingFrameworkParameters(
        maxCuts: Int,
        maxSubcircuits: Int,
        maxSubcircuitWidth: Int,
        subcircuitSizeImbalance: Double
    )

    final case class DeviceEffectiveWidth(
        device: Device,
        rawWidth: Int,
        effectiveWidth: Int,
        regionQuality: Double,
        physicalRegion: List[Int]
    )

    final case class CutLocation(
        gateIndex: Int,
        gateName: String,
        qubits: List[Int],
        overhead: Double
    )

    final case class SubcircuitAssignment(
        subcircuitIndex: Int,
        device: Device,
        logicalQubits: List[Int],
        physicalQubits: List[Int],
        estimatedFidelity: Double,
        routingSwapCost: Double,
        estimatedQueueRunMillis: Long
    )

    final case class CuttingPlanMetrics(
        estimatedFidelity: Double,
        samplingOverhead: Double,
        classicalReconstructionCost: Double,
        classicalMemoryBytes: Double,
        estimatedHardwareError: Double,
        routingSwapCost: Double,
        queueRunMillis: Long,
        shotsRequired: Long,
        feasible: Boolean,
        constraintViolations: List[String]
    )

    final case class CuttingPlan(
        name: String,
        subcircuits: List[Circuit],
        cutLocations: List[CutLocation],
        assignments: List[SubcircuitAssignment],
        deviceWidths: List[DeviceEffectiveWidth],
        parameters: CuttingFrameworkParameters,
        metrics: CuttingPlanMetrics,
        score: Double,
        explanation: List[String]
    ) {
        def cuts: Int = cutLocations.size
        def subcircuitCount: Int = subcircuits.size
    }

    final case class CuttingDecision(
        selected: CuttingPlan,
        frontier: List[CuttingPlan],
        candidates: List[CuttingPlan]
    )
}
