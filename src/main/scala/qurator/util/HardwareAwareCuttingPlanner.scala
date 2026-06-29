package qurator.util

import cats.MonadThrow
import cats.syntax.all._
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.cutting._
import qurator.domain.device.Device

object HardwareAwareCuttingPlanner {

    final case class Config(
        effectiveWidthFidelityThreshold: Double = 0.96,
        maxCandidateWidths: Int = 6,
        cutGateOverhead: Double = 9.0,
        multiQubitCutOverhead: Double = 16.0,
        reconstructionVariancePenalty: Double = 0.50,
        cutFidelityPenalty: Double = 0.01,
        queueLengthMillisFactor: Long = 1000L,
        infeasiblePenalty: Double = 1000.0,
        unroutableSwapPenalty: Double = 100.0,
        queueScaleMillis: Double = 60.0 * 60.0 * 1000.0
    )

    private final case class Topology(
        qubits: Vector[Int],
        edges: Set[(Int, Int)]
    ) {
        lazy val neighbors: Map[Int, Vector[Int]] = {
            val init = qubits.map(_ -> Vector.empty[Int]).toMap
            edges.foldLeft(init) { case (acc, (a, b)) =>
                acc.updated(a, (acc.getOrElse(a, Vector.empty) :+ b).distinct.sorted)
                    .updated(b, (acc.getOrElse(b, Vector.empty) :+ a).distinct.sorted)
            }
        }

        def hasEdge(a: Int, b: Int): Boolean =
            edges.contains(edgeKey(a, b))

        def shortestPathLength(start: Int, goal: Int): Option[Int] = {
            if (start == goal) Some(0)
            else if (!qubits.contains(start) || !qubits.contains(goal)) None
            else {
                @annotation.tailrec
                def loop(frontier: Vector[(Int, Int)], seen: Set[Int]): Option[Int] =
                    if (frontier.isEmpty) None
                    else {
                        val (current, dist) = frontier.head
                        val rest = frontier.tail
                        val next = neighbors.getOrElse(current, Vector.empty).filterNot(seen.contains)
                        if (next.contains(goal)) Some(dist + 1)
                        else loop(rest ++ next.map(_ -> (dist + 1)), seen ++ next)
                    }

                loop(Vector(start -> 0), Set(start))
            }
        }
    }

    private final case class DeviceModel(
        device: Device,
        calibration: Option[CanonicalCalibration],
        topology: Option[Topology],
        effectiveWidth: DeviceEffectiveWidth
    )

    private final case class InteractionEdge(
        a: Int,
        b: Int,
        weight: Double
    )

    private final case class PartitionCandidate(
        name: String,
        groups: List[List[Int]]
    )

    private final case class BuiltSubcircuits(
        subcircuits: List[Circuit],
        logicalGroups: List[List[Int]],
        cutLocations: List[CutLocation]
    )

    private final case class AssignmentEval(
        assignment: SubcircuitAssignment,
        withinEffectiveWidth: Boolean
    )

    def plan[F[_]: MonadThrow](
        request: CuttingRequest,
        fetchCalibration: Device => F[DeviceCalibration],
        compileCircuitFor: (Device, Circuit) => F[Circuit],
        config: Config = Config()
    ): F[CuttingDecision] =
        if (request.devices.isEmpty) {
            noCutDecision(request, "no-device-candidate").pure[F]
        } else {
            request.devices
                .traverse(loadDeviceModel(_, fetchCalibration, config))
                .flatMap { deviceModels =>
                    val partitions = candidatePartitions(request.circuit, deviceModels, config)

                    partitions
                        .traverse { candidate =>
                            val built = buildSubcircuits(request.circuit, candidate, config)
                            buildPlan(request, candidate.name, built, deviceModels, compileCircuitFor, config)
                        }
                        .map(decisionFromCandidates(request, _))
                }
        }

    def noCutDecision(
        request: CuttingRequest,
        name: String = "no-cut"
    ): CuttingDecision = {
        val plan = staticPlan(
            request = request,
            name = name,
            subcircuits = List(request.circuit),
            cutLocations = Nil,
            explanation = List("No cutting was requested; the original circuit is submitted unchanged.")
        )

        CuttingDecision(plan, List(plan), List(plan))
    }

    def decisionFromSubcircuits(
        request: CuttingRequest,
        name: String,
        subcircuits: List[Circuit],
        explanation: List[String]
    ): CuttingDecision = {
        val fallbackSubcircuits =
            if (subcircuits.nonEmpty) subcircuits else List(request.circuit)

        val plan = staticPlan(
            request = request,
            name = name,
            subcircuits = fallbackSubcircuits,
            cutLocations = Nil,
            explanation = explanation
        )

        CuttingDecision(plan, List(plan), List(plan))
    }

    private def staticPlan(
        request: CuttingRequest,
        name: String,
        subcircuits: List[Circuit],
        cutLocations: List[CutLocation],
        explanation: List[String]
    ): CuttingPlan = {
        val widths = subcircuits.map(_.qubits)
        val maxWidth = widths.maxOption.getOrElse(request.circuit.qubits)
        val minWidth = widths.minOption.getOrElse(maxWidth)
        val cuts = cutLocations.size
        val samplingOverhead = if (cuts == 0) 1.0 else cappedPow(9.0, cuts)
        val shots = scaledShots(request.shots, samplingOverhead)
        val memoryBytes = cappedPow(2.0, maxWidth) * math.max(1.0, samplingOverhead) * 16.0

        CuttingPlan(
            name = name,
            subcircuits = subcircuits,
            cutLocations = cutLocations,
            assignments = Nil,
            deviceWidths = Nil,
            parameters = CuttingFrameworkParameters(
                maxCuts = cuts,
                maxSubcircuits = subcircuits.size,
                maxSubcircuitWidth = maxWidth,
                subcircuitSizeImbalance = sizeImbalance(maxWidth, minWidth)
            ),
            metrics = CuttingPlanMetrics(
                estimatedFidelity = 1.0,
                samplingOverhead = samplingOverhead,
                classicalReconstructionCost = samplingOverhead * math.max(1, subcircuits.size).toDouble,
                classicalMemoryBytes = memoryBytes,
                estimatedHardwareError = 0.0,
                routingSwapCost = 0.0,
                queueRunMillis = 0L,
                shotsRequired = shots,
                feasible = true,
                constraintViolations = Nil
            ),
            score = 1.0 - math.log(samplingOverhead),
            explanation = explanation
        )
    }

    private def loadDeviceModel[F[_]: MonadThrow](
        device: Device,
        fetchCalibration: Device => F[DeviceCalibration],
        config: Config
    ): F[DeviceModel] =
        fetchCalibration(device).attempt.map {
            case Right(rawCalibration) =>
                val canonical = FidelityEstimator.normalizeCalibration(rawCalibration)
                val topology = topologyOf(rawCalibration).map { t =>
                    Topology(t.qubits.toVector.distinct.sorted, t.normalizedEdges.map { case (a, b) => edgeKey(a, b) }.toSet)
                }
                val effective = inferEffectiveWidth(device, rawCalibration, canonical, topology, config)

                DeviceModel(
                    device = device,
                    calibration = Some(canonical),
                    topology = topology,
                    effectiveWidth = effective
                )

            case Left(_) =>
                val region = (0 until device.qubits).toList
                DeviceModel(
                    device = device,
                    calibration = None,
                    topology = None,
                    effectiveWidth = DeviceEffectiveWidth(
                        device = device,
                        rawWidth = device.qubits,
                        effectiveWidth = device.qubits,
                        regionQuality = 0.0,
                        physicalRegion = region
                    )
                )
        }

    private def inferEffectiveWidth(
        device: Device,
        rawCalibration: DeviceCalibration,
        calibration: CanonicalCalibration,
        topology: Option[Topology],
        config: Config
    ): DeviceEffectiveWidth =
        topology match {
            case None =>
                DeviceEffectiveWidth(
                    device = device,
                    rawWidth = device.qubits,
                    effectiveWidth = device.qubits,
                    regionQuality = averageDeviceQuality(calibration),
                    physicalRegion = (0 until device.qubits).toList
                )

            case Some(t) if t.edges.isEmpty =>
                DeviceEffectiveWidth(
                    device = device,
                    rawWidth = device.qubits,
                    effectiveWidth = t.qubits.size.max(device.qubits),
                    regionQuality = averageDeviceQuality(calibration),
                    physicalRegion = (if (t.qubits.nonEmpty) t.qubits.toList else (0 until device.qubits).toList)
                )

            case Some(t) =>
                val edgeQualityOverrides = rawEdgeQualities(rawCalibration)
                val best = t.qubits.map(seed => greedyEffectiveRegion(seed, t, calibration, edgeQualityOverrides, config)).maxByOption {
                    case (region, quality) => (region.size, quality)
                }

                val (region, quality) =
                    best.getOrElse(Vector.empty[Int] -> 0.0)

                DeviceEffectiveWidth(
                    device = device,
                    rawWidth = device.qubits,
                    effectiveWidth = region.size.max(1),
                    regionQuality = quality,
                    physicalRegion = region.toList.sorted
                )
        }

    private def greedyEffectiveRegion(
        seed: Int,
        topology: Topology,
        calibration: CanonicalCalibration,
        edgeQualityOverrides: Map[(Int, Int), Double],
        config: Config
    ): (Vector[Int], Double) = {
        var region = Set(seed)
        var quality = regionQuality(region, topology, calibration, edgeQualityOverrides)
        var keepGoing = true

        while (keepGoing) {
            val boundary =
                region.toVector
                    .flatMap(q => topology.neighbors.getOrElse(q, Vector.empty))
                    .filterNot(region.contains)
                    .distinct

            val best = boundary.map { q =>
                val next = region + q
                (q, regionQuality(next, topology, calibration, edgeQualityOverrides))
            }.sortBy { case (q, qlty) => (qlty, -q) }.lastOption

            best match {
                case Some((q, qlty)) if qlty >= config.effectiveWidthFidelityThreshold =>
                    region = region + q
                    quality = qlty

                case _ =>
                    keepGoing = false
            }
        }

        if (quality >= config.effectiveWidthFidelityThreshold) {
            (region.toVector.sorted, quality)
        } else {
            val singleQuality = regionQuality(Set(seed), topology, calibration, edgeQualityOverrides)
            (Vector(seed), singleQuality)
        }
    }

    private def rawEdgeQualities(rawCalibration: DeviceCalibration): Map[(Int, Int), Double] =
        edgeMetricsOf(rawCalibration).flatMap { case (edge, metrics) =>
            val fidelity =
                metrics.gateFidelities.values
                    .filter(v => v.isFinite && v > 0.0)
                    .map(probabilityLike)
                    .maxOption
                    .orElse {
                        metrics.gateErrors.values
                            .filter(v => v.isFinite && v >= 0.0)
                            .map(v => 1.0 - probabilityLike(v))
                            .maxOption
                    }
                    .map(clamp01)

            fidelity.map(f => edgeKey(edge) -> f)
        }

    private def probabilityLike(value: Double): Double =
        if (value > 1.0 && value <= 100.0) value / 100.0 else value

    private def averageDeviceQuality(calibration: CanonicalCalibration): Double = {
        val f1 = calibration.eps1qAvg.map(e => clamp01(1.0 - e)).getOrElse(0.995)
        val f2 = calibration.eps2qAvg.map(e => clamp01(1.0 - e)).getOrElse(0.99)
        val ro = calibration.readoutFidelityAvg.map(clamp01).getOrElse(0.99)
        (f1 + f2 + ro) / 3.0
    }

    private def regionQuality(
        region: Set[Int],
        topology: Topology,
        calibration: CanonicalCalibration,
        edgeQualityOverrides: Map[(Int, Int), Double]
    ): Double = {
        val nodeQuality =
            if (region.isEmpty) 0.0
            else region.toVector.map(q => qubitQuality(q, calibration)).sum / region.size.toDouble

        val internalEdges = topology.edges.filter { case (a, b) => region.contains(a) && region.contains(b) }.toVector
        val edgeQuality =
            if (region.size <= 1) nodeQuality
            else if (internalEdges.isEmpty) 0.0
            else internalEdges.map { case (a, b) => twoQubitQuality(a, b, calibration, edgeQualityOverrides) }.sum / internalEdges.size.toDouble

        clamp01(0.45 * nodeQuality + 0.55 * edgeQuality)
    }

    private def qubitQuality(q: Int, calibration: CanonicalCalibration): Double = {
        val f1 = calibration.eps1q
            .collect { case ((qq, _), error) if qq == q => clamp01(1.0 - error) }
            .toVector
            .average
            .orElse(calibration.eps1qAvg.map(e => clamp01(1.0 - e)))
            .getOrElse(0.995)

        val readout = calibration.readoutFidelity.get(q).orElse(calibration.readoutFidelityAvg).map(clamp01).getOrElse(0.99)

        0.55 * f1 + 0.45 * readout
    }

    private def twoQubitQuality(
        a: Int,
        b: Int,
        calibration: CanonicalCalibration,
        edgeQualityOverrides: Map[(Int, Int), Double]
    ): Double = {
        val key = edgeKey(a, b)
        val qualities =
            calibration.eps2q.collect {
                case ((edge, _), error) if edge == key => clamp01(1.0 - error)
            }.toVector

        edgeQualityOverrides.get(key)
            .orElse(qualities.average)
            .orElse(calibration.eps2qAvg.map(e => clamp01(1.0 - e)))
            .getOrElse(0.99)
    }

    private def candidatePartitions(
        circuit: Circuit,
        deviceModels: List[DeviceModel],
        config: Config
    ): List[PartitionCandidate] = {
        val noCut = PartitionCandidate("no-cut", List((0 until circuit.qubits).toList))
        val maxEffectiveWidth = deviceModels.map(_.effectiveWidth.effectiveWidth).maxOption.getOrElse(circuit.qubits).max(1)
        val minEffectiveWidth = deviceModels.map(_.effectiveWidth.effectiveWidth).filter(_ > 0).minOption.getOrElse(maxEffectiveWidth)
        val baseWidths =
            List(
                maxEffectiveWidth,
                math.ceil(maxEffectiveWidth.toDouble * 0.75).toInt,
                math.ceil(maxEffectiveWidth.toDouble * 0.50).toInt,
                minEffectiveWidth
            )
                .filter(w => w > 0 && w < circuit.qubits)
                .distinct
                .sortBy(w => -w)
                .take(config.maxCandidateWidths)

        val interactions = interactionEdges(circuit, config)

        val cutCandidates =
            baseWidths.zipWithIndex.map { case (width, idx) =>
                val groups = refinePartition(greedyPartition(circuit.qubits, width, interactions), width, interactions)
                val label =
                    if (idx == 0) "conservative"
                    else if (idx == baseWidths.size - 1) "aggressive"
                    else "balanced"

                PartitionCandidate(s"$label-width-$width", groups)
            }

        (noCut :: cutCandidates).distinctBy(_.groups.map(_.sorted))
    }

    private def greedyPartition(
        qubits: Int,
        targetWidth: Int,
        interactions: List[InteractionEdge]
    ): List[List[Int]] = {
        val importance =
            interactions.foldLeft(Map.empty[Int, Double].withDefaultValue(0.0)) { (acc, edge) =>
                acc.updated(edge.a, acc(edge.a) + edge.weight)
                    .updated(edge.b, acc(edge.b) + edge.weight)
            }

        var unassigned = (0 until qubits).toSet
        var groups = Vector.empty[Vector[Int]]

        while (unassigned.nonEmpty) {
            val seed = unassigned.toVector.maxBy(q => (importance(q), -q))
            var group = Vector(seed)
            unassigned = unassigned - seed

            while (group.size < targetWidth && unassigned.nonEmpty) {
                val next = unassigned.toVector.maxBy { q =>
                    val connection = group.map(g => interactionWeight(q, g, interactions)).sum
                    (connection, importance(q), -q)
                }
                group = group :+ next
                unassigned = unassigned - next
            }

            groups = groups :+ group.sorted
        }

        groups.map(_.toList).toList
    }

    private def refinePartition(
        initial: List[List[Int]],
        targetWidth: Int,
        interactions: List[InteractionEdge]
    ): List[List[Int]] = {
        var groups = initial.map(_.toVector).toVector
        var improved = true
        var rounds = 0

        while (improved && rounds < 3) {
            improved = false
            rounds += 1

            val allQubits = groups.flatten.distinct
            allQubits.foreach { q =>
                val currentIdx = groups.indexWhere(_.contains(q))
                val currentCost = partitionCutWeight(groups.map(_.toList).toList, interactions)

                val bestMove =
                    groups.indices
                        .filter(i => i != currentIdx && groups(i).size < targetWidth)
                        .flatMap { targetIdx =>
                            val moved =
                                groups.zipWithIndex.map {
                                    case (g, idx) if idx == currentIdx => g.filterNot(_ == q)
                                    case (g, idx) if idx == targetIdx  => (g :+ q).sorted
                                    case (g, _)                        => g
                                }.filter(_.nonEmpty)

                            val movedCost = partitionCutWeight(moved.map(_.toList).toList, interactions)
                            Option.when(movedCost + 1e-9 < currentCost)(targetIdx -> moved)
                        }
                        .sortBy { case (_, moved) => partitionCutWeight(moved.map(_.toList).toList, interactions) }
                        .headOption

                bestMove.foreach { case (_, moved) =>
                    groups = moved
                    improved = true
                }
            }
        }

        groups.map(_.toList.sorted).toList
    }

    private def buildSubcircuits(
        circuit: Circuit,
        candidate: PartitionCandidate,
        config: Config
    ): BuiltSubcircuits = {
        val groups = candidate.groups.map(_.sorted)
        val groupByQubit =
            groups.zipWithIndex.flatMap { case (group, idx) => group.map(_ -> idx) }.toMap

        val cutLocations =
            circuit.remainingGates.zipWithIndex.flatMap { case (gate, idx) =>
                val qs = gateQubits(gate)
                val touchedGroups = qs.flatMap(groupByQubit.get).distinct
                Option.when(qs.size >= 2 && touchedGroups.size > 1) {
                    CutLocation(
                        gateIndex = idx,
                        gateName = gateName(gate),
                        qubits = qs.toList.sorted,
                        overhead = overheadForGate(gate, config)
                    )
                }
            }

        val subcircuits =
            groups.zipWithIndex.map { case (group, groupIdx) =>
                val remap = group.zipWithIndex.toMap
                val gates =
                    circuit.remainingGates.flatMap { gate =>
                        val qs = gateQubits(gate)
                        if (qs.isEmpty && groupIdx == 0) remapGate(gate, remap)
                        else if (qs.nonEmpty && qs.forall(remap.contains)) remapGate(gate, remap)
                        else None
                    }

                Circuit(
                    remainingGates = gates,
                    qubits = group.size,
                    name = subcircuitName(circuit, candidate.name, groupIdx)
                )
            }

        BuiltSubcircuits(subcircuits, groups, cutLocations)
    }

    private def buildPlan[F[_]: MonadThrow](
        request: CuttingRequest,
        name: String,
        built: BuiltSubcircuits,
        deviceModels: List[DeviceModel],
        compileCircuitFor: (Device, Circuit) => F[Circuit],
        config: Config
    ): F[CuttingPlan] =
        built.subcircuits.zipWithIndex.traverse { case (subcircuit, idx) =>
            assignSubcircuit(subcircuit, idx, built.logicalGroups(idx), deviceModels, compileCircuitFor, config)
        }.map { assignments =>
            scorePlan(request, name, built, assignments, deviceModels.map(_.effectiveWidth), config)
        }

    private def assignSubcircuit[F[_]: MonadThrow](
        subcircuit: Circuit,
        subcircuitIndex: Int,
        logicalGroup: List[Int],
        deviceModels: List[DeviceModel],
        compileCircuitFor: (Device, Circuit) => F[Circuit],
        config: Config
    ): F[AssignmentEval] = {
        val effectiveEligible =
            deviceModels.filter(m => m.effectiveWidth.effectiveWidth >= subcircuit.qubits && m.device.qubits >= subcircuit.qubits)

        val rawEligible =
            deviceModels.filter(_.device.qubits >= subcircuit.qubits)

        val considered =
            if (effectiveEligible.nonEmpty) effectiveEligible
            else if (rawEligible.nonEmpty) rawEligible
            else deviceModels

        considered
            .traverse(m => evaluateAssignment(m, subcircuit, subcircuitIndex, logicalGroup, compileCircuitFor, config))
            .map(_.maxBy { eval =>
                val a = eval.assignment
                val widthOk = if (eval.withinEffectiveWidth) 1.0 else 0.0
                (widthOk, a.estimatedFidelity, -a.routingSwapCost, -a.estimatedQueueRunMillis.toDouble, a.device.qubits.toDouble)
            })
    }

    private def evaluateAssignment[F[_]: MonadThrow](
        model: DeviceModel,
        subcircuit: Circuit,
        subcircuitIndex: Int,
        logicalGroup: List[Int],
        compileCircuitFor: (Device, Circuit) => F[Circuit],
        config: Config
    ): F[AssignmentEval] = {
        val physicalQubits =
            if (model.effectiveWidth.physicalRegion.size >= subcircuit.qubits)
                model.effectiveWidth.physicalRegion.take(subcircuit.qubits)
            else
                (0 until subcircuit.qubits).toList

        val routingCost = routingSwapCost(subcircuit, model.topology, physicalQubits, config)
        val withinEffectiveWidth = model.effectiveWidth.effectiveWidth >= subcircuit.qubits

        val estimateF =
            model.calibration match {
                case Some(calibration) if subcircuit.remainingGates.nonEmpty && canUseFidelityEstimator(subcircuit) =>
                    compileCircuitFor(model.device, subcircuit).map { compiled =>
                        val fidelity = FidelityEstimator.score(compiled, calibration).pTotal
                        val runMillis = estimatedRunMillis(compiled, calibration, model.device, config)
                        fidelity -> runMillis
                    }

                case Some(calibration) if subcircuit.remainingGates.nonEmpty =>
                    compileCircuitFor(model.device, subcircuit).map { compiled =>
                        val fidelity = fallbackFidelity(compiled, calibration)
                        val runMillis = estimatedRunMillis(compiled, calibration, model.device, config)
                        fidelity -> runMillis
                    }

                case Some(calibration) =>
                    val runMillis = estimatedRunMillis(subcircuit, calibration, model.device, config)
                    (1.0, runMillis).pure[F]

                case None =>
                    val fallbackFidelity =
                        if (withinEffectiveWidth) 0.90 else 0.50
                    val runMillis = model.device.queueLength.toLong * config.queueLengthMillisFactor
                    (fallbackFidelity, runMillis).pure[F]
            }

        estimateF.map { case (fidelity, queueRunMillis) =>
            AssignmentEval(
                assignment = SubcircuitAssignment(
                    subcircuitIndex = subcircuitIndex,
                    device = model.device,
                    logicalQubits = logicalGroup,
                    physicalQubits = physicalQubits,
                    estimatedFidelity = clamp01(fidelity),
                    routingSwapCost = routingCost,
                    estimatedQueueRunMillis = queueRunMillis
                ),
                withinEffectiveWidth = withinEffectiveWidth
            )
        }
    }

    private def scorePlan(
        request: CuttingRequest,
        name: String,
        built: BuiltSubcircuits,
        assignmentEvals: List[AssignmentEval],
        effectiveWidths: List[DeviceEffectiveWidth],
        config: Config
    ): CuttingPlan = {
        val assignments = assignmentEvals.map(_.assignment)
        val cuts = built.cutLocations.size
        val samplingOverhead =
            if (built.cutLocations.isEmpty) 1.0
            else built.cutLocations.map(_.overhead).foldLeft(1.0)(cappedProduct)

        val widths = built.subcircuits.map(_.qubits)
        val maxWidth = widths.maxOption.getOrElse(request.circuit.qubits)
        val minWidth = widths.minOption.getOrElse(maxWidth)
        val baseShots = request.shots.getOrElse(1000L).max(1L)
        val shotsRequired = scaledShots(Some(baseShots), samplingOverhead)
        val subFidelityProduct =
            assignments.map(a => math.log(math.max(1e-12, a.estimatedFidelity))).sum
        val productFidelity = math.exp(subFidelityProduct)
        val reconstructionPenalty =
            math.exp(-config.reconstructionVariancePenalty * math.log1p(samplingOverhead) / math.sqrt(baseShots.toDouble))
        val cutPenalty = math.exp(-config.cutFidelityPenalty * cuts.toDouble)
        val estimatedFidelity = clamp01(productFidelity * reconstructionPenalty * cutPenalty)
        val routingCost = assignments.map(_.routingSwapCost).sum
        val queueRunMillis = assignments.map(_.estimatedQueueRunMillis).maxOption.getOrElse(0L)
        val classicalReconstructionCost = samplingOverhead * math.max(1, built.subcircuits.size).toDouble
        val classicalMemoryBytes = classicalReconstructionCost * cappedPow(2.0, maxWidth) * 16.0
        val widthViolations =
            assignmentEvals.collect {
                case eval if !eval.withinEffectiveWidth =>
                    val a = eval.assignment
                    s"subcircuit ${a.subcircuitIndex} width ${built.subcircuits(a.subcircuitIndex).qubits} exceeds effective width on ${a.device.platform}/${a.device.platformId}"
            }

        val budgetViolations =
            List(
                request.budgets.maxSamplingOverhead.filter(_ < samplingOverhead).map(v => s"sampling overhead $samplingOverhead exceeds budget $v"),
                request.budgets.maxClassicalMemoryBytes.filter(_ < classicalMemoryBytes).map(v => s"classical memory $classicalMemoryBytes exceeds budget $v"),
                request.budgets.maxShots.filter(_ < shotsRequired).map(v => s"shots $shotsRequired exceeds budget $v"),
                request.budgets.maxQueueRunMillis.filter(_ < queueRunMillis).map(v => s"queue/run $queueRunMillis ms exceeds budget $v ms")
            ).flatten

        val noAssignmentViolation =
            Option.when(assignments.size != built.subcircuits.size)("not every subcircuit could be assigned to a device").toList

        val violations = widthViolations ++ budgetViolations ++ noAssignmentViolation
        val feasible = violations.isEmpty
        val metrics =
            CuttingPlanMetrics(
                estimatedFidelity = estimatedFidelity,
                samplingOverhead = samplingOverhead,
                classicalReconstructionCost = classicalReconstructionCost,
                classicalMemoryBytes = classicalMemoryBytes,
                estimatedHardwareError = clamp01(1.0 - productFidelity),
                routingSwapCost = routingCost,
                queueRunMillis = queueRunMillis,
                shotsRequired = shotsRequired,
                feasible = feasible,
                constraintViolations = violations
            )

        val weights = request.objectiveWeights
        val score =
            estimatedFidelity -
                weights.samplingOverhead * math.log(math.max(1.0, samplingOverhead)) -
                weights.classicalReconstruction * (math.log1p(classicalReconstructionCost) / 10.0) -
                weights.hardwareError * metrics.estimatedHardwareError -
                weights.routingSwap * math.log1p(routingCost) -
                weights.queueRun * (math.log1p(queueRunMillis.toDouble) / math.log1p(config.queueScaleMillis)) -
                (if (feasible) 0.0 else config.infeasiblePenalty)

        CuttingPlan(
            name = name,
            subcircuits = built.subcircuits,
            cutLocations = built.cutLocations,
            assignments = assignments,
            deviceWidths = effectiveWidths,
            parameters = CuttingFrameworkParameters(
                maxCuts = cuts,
                maxSubcircuits = built.subcircuits.size,
                maxSubcircuitWidth = maxWidth,
                subcircuitSizeImbalance = sizeImbalance(maxWidth, minWidth)
            ),
            metrics = metrics,
            score = score,
            explanation = planExplanation(name, built, metrics)
        )
    }

    private def decisionFromCandidates(
        request: CuttingRequest,
        candidates: List[CuttingPlan]
    ): CuttingDecision = {
        val nonEmptyCandidates =
            if (candidates.nonEmpty) candidates else List(noCutDecision(request).selected)

        val selectedPool =
            nonEmptyCandidates.filter(_.metrics.feasible) match {
                case Nil => nonEmptyCandidates
                case xs  => xs
            }

        val selected = selectedPool.maxBy(p => (p.score, p.metrics.estimatedFidelity))
        val pareto =
            nonEmptyCandidates
                .filterNot(p => nonEmptyCandidates.exists(other => other != p && dominates(other, p)))
                .sortBy(p => (-p.score, p.metrics.samplingOverhead, p.metrics.queueRunMillis))

        val frontier =
            (selected :: pareto.filterNot(_ == selected))
                .distinctBy(p => (p.parameters, p.cutLocations.map(_.gateIndex), p.assignments.map(_.device.platformId)))
                .take(request.paretoLimit.max(1))

        CuttingDecision(
            selected = selected,
            frontier = frontier,
            candidates = nonEmptyCandidates.sortBy(p => (-p.score, p.metrics.samplingOverhead))
        )
    }

    private def dominates(a: CuttingPlan, b: CuttingPlan): Boolean = {
        val betterOrEqual =
            (!b.metrics.feasible || a.metrics.feasible) &&
                a.metrics.estimatedFidelity >= b.metrics.estimatedFidelity - 1e-12 &&
                a.metrics.samplingOverhead <= b.metrics.samplingOverhead + 1e-12 &&
                a.metrics.classicalMemoryBytes <= b.metrics.classicalMemoryBytes + 1e-12 &&
                a.metrics.routingSwapCost <= b.metrics.routingSwapCost + 1e-12 &&
                a.metrics.queueRunMillis <= b.metrics.queueRunMillis &&
                a.cuts <= b.cuts

        val strictlyBetter =
            (a.metrics.feasible && !b.metrics.feasible) ||
                a.metrics.estimatedFidelity > b.metrics.estimatedFidelity + 1e-12 ||
                a.metrics.samplingOverhead < b.metrics.samplingOverhead - 1e-12 ||
                a.metrics.classicalMemoryBytes < b.metrics.classicalMemoryBytes - 1e-12 ||
                a.metrics.routingSwapCost < b.metrics.routingSwapCost - 1e-12 ||
                a.metrics.queueRunMillis < b.metrics.queueRunMillis ||
                a.cuts < b.cuts

        betterOrEqual && strictlyBetter
    }

    private def interactionEdges(circuit: Circuit, config: Config): List[InteractionEdge] = {
        val n = circuit.remainingGates.size.max(1).toDouble
        val weightedPairs =
            circuit.remainingGates.zipWithIndex.flatMap { case (gate, idx) =>
                val qs = gateQubits(gate).distinct
                val depthWeight = 1.0 + idx.toDouble / n
                val gateWeight = math.log(overheadForGate(gate, config)).max(1.0)

                qs.combinations(2).collect {
                    case Vector(a, b) =>
                        val (x, y) = edgeKey(a, b)
                        InteractionEdge(x, y, depthWeight * gateWeight)
                }.toList
            }

        weightedPairs
            .groupBy(e => edgeKey(e.a, e.b))
            .toList
            .map { case ((a, b), edges) => InteractionEdge(a, b, edges.map(_.weight).sum) }
    }

    private def interactionWeight(a: Int, b: Int, interactions: List[InteractionEdge]): Double = {
        val key = edgeKey(a, b)
        interactions.collectFirst {
            case InteractionEdge(x, y, weight) if edgeKey(x, y) == key => weight
        }.getOrElse(0.0)
    }

    private def partitionCutWeight(groups: List[List[Int]], interactions: List[InteractionEdge]): Double = {
        val groupByQubit =
            groups.zipWithIndex.flatMap { case (g, idx) => g.map(_ -> idx) }.toMap

        interactions.collect {
            case InteractionEdge(a, b, weight) if groupByQubit.get(a) != groupByQubit.get(b) => weight
        }.sum
    }

    private def routingSwapCost(
        circuit: Circuit,
        topology: Option[Topology],
        physicalQubits: List[Int],
        config: Config
    ): Double =
        topology match {
            case None => 0.0
            case Some(t) if t.edges.isEmpty => 0.0
            case Some(t) =>
                val logicalToPhysical = physicalQubits.zipWithIndex.map { case (p, l) => l -> p }.toMap
                circuit.remainingGates.flatMap { gate =>
                    gateQubits(gate).distinct match {
                        case Vector(a, b) =>
                            for {
                                pa <- logicalToPhysical.get(a)
                                pb <- logicalToPhysical.get(b)
                            } yield {
                                if (t.hasEdge(pa, pb)) 0.0
                                else {
                                    t.shortestPathLength(pa, pb) match {
                                        case Some(distance) => math.max(0, distance - 1).toDouble
                                        case None           => config.unroutableSwapPenalty
                                    }
                                }
                            }

                        case qs if qs.size > 2 =>
                            val pairs = qs.combinations(2).toList
                            Some(
                                pairs.flatMap {
                                    case Vector(a, b) =>
                                        for {
                                            pa <- logicalToPhysical.get(a)
                                            pb <- logicalToPhysical.get(b)
                                        } yield {
                                            t.shortestPathLength(pa, pb).map(d => math.max(0, d - 1).toDouble).getOrElse(config.unroutableSwapPenalty)
                                        }
                                    case _ => None
                                }.sum
                            )

                        case _ => None
                    }
                }.sum
        }

    private def estimatedRunMillis(
        circuit: Circuit,
        calibration: CanonicalCalibration,
        device: Device,
        config: Config
    ): Long = {
        val gateDurationNs =
            circuit.remainingGates.foldLeft(0L) { (acc, gate) =>
                acc + calibration.durationNsFor(gate)
            }

        val runMillis = math.ceil(gateDurationNs.toDouble / 1000000.0).toLong
        device.queueLength.toLong * config.queueLengthMillisFactor + runMillis
    }

    private def canUseFidelityEstimator(circuit: Circuit): Boolean =
        circuit.remainingGates.forall {
            case X(_) | H(_) | SX(_) | Measure(_) | RX(_, _) | RZ(_, _) | RY(_, _) => true
            case CX(_, _) | CZ(_, _) | Swap(_, _) | CRZ(_, _, _)                  => true
            case _                                                                 => false
        }

    private def fallbackFidelity(
        circuit: Circuit,
        calibration: CanonicalCalibration
    ): Double = {
        val logP =
            circuit.remainingGates.foldLeft(0.0) { (acc, gate) =>
                gate match {
                    case Measure(q) =>
                        val readout = calibration.readoutFidelity.get(q).orElse(calibration.readoutFidelityAvg).getOrElse(0.99)
                        acc + math.log(math.max(1e-12, clamp01(readout)))

                    case _ =>
                        val arity = gateQubits(gate).distinct.size
                        val eps =
                            if (arity <= 0) 0.0
                            else if (arity == 1) calibration.eps1qAvg.getOrElse(calibration.epsFor(gate)).max(0.0)
                            else calibration.eps2qAvg.getOrElse(calibration.epsFor(gate)).max(0.0)

                        acc + math.log1p(-clamp01(eps))
                }
            }

        clamp01(math.exp(logP))
    }

    private def planExplanation(
        name: String,
        built: BuiltSubcircuits,
        metrics: CuttingPlanMetrics
    ): List[String] =
        List(
            s"Generated $name plan with ${built.cutLocations.size} cut(s) and ${built.subcircuits.size} subcircuit(s).",
            s"Estimated sampling overhead is ${metrics.samplingOverhead}; estimated fidelity is ${metrics.estimatedFidelity}."
        ) ++ Option.when(metrics.constraintViolations.nonEmpty)(
            s"Constraint violations: ${metrics.constraintViolations.mkString("; ")}"
        ).toList

    private def subcircuitName(circuit: Circuit, planName: String, index: Int): String = {
        val prefix = Option(circuit.name).filter(_.nonEmpty).getOrElse("circuit")
        s"${prefix}_${planName}_sub$index"
    }

    private def edgeKey(a: Int, b: Int): (Int, Int) =
        if (a <= b) (a, b) else (b, a)

    private def edgeKey(edge: (Int, Int)): (Int, Int) =
        edgeKey(edge._1, edge._2)

    private def clamp01(x: Double): Double =
        math.max(0.0, math.min(1.0, x))

    private def sizeImbalance(maxWidth: Int, minWidth: Int): Double =
        if (maxWidth <= 0) 0.0 else (maxWidth - minWidth).toDouble / maxWidth.toDouble

    private def cappedPow(base: Double, exponent: Int): Double =
        if (exponent <= 0) 1.0
        else math.exp(math.min(700.0, math.log(base) * exponent.toDouble))

    private def cappedProduct(a: Double, b: Double): Double =
        if (a.isInfinity || b.isInfinity) Double.PositiveInfinity
        else math.exp(math.min(700.0, math.log(a) + math.log(b)))

    private def scaledShots(shots: Option[Long], overhead: Double): Long = {
        val base = shots.getOrElse(1000L).max(1L).toDouble
        val scaled = base * overhead
        if (!scaled.isFinite || scaled >= Long.MaxValue.toDouble) Long.MaxValue
        else math.ceil(scaled).toLong
    }

    private def overheadForGate(gate: Gate, config: Config): Double = {
        val arity = gateQubits(gate).distinct.size
        if (arity <= 1) 1.0
        else if (arity == 2) config.cutGateOverhead
        else config.multiQubitCutOverhead
    }

    private def gateName(gate: Gate): String =
        gate match {
            case X(_)                    => "X"
            case Y(_)                    => "Y"
            case Z(_)                    => "Z"
            case H(_)                    => "H"
            case S(_)                    => "S"
            case SDG(_)                  => "SDG"
            case T(_)                    => "T"
            case TDG(_)                  => "TDG"
            case SX(_)                   => "SX"
            case SXDG(_)                 => "SXDG"
            case Id(_)                   => "ID"
            case Phase(_, _)             => "PHASE"
            case RX(_, _)                => "RX"
            case RY(_, _)                => "RY"
            case RZ(_, _)                => "RZ"
            case U(_, _, _, _)           => "U"
            case U2(_, _, _)             => "U2"
            case U3(_, _, _, _)          => "U3"
            case CX(_, _)                => "CX"
            case CY(_, _)                => "CY"
            case CZ(_, _)                => "CZ"
            case CH(_, _)                => "CH"
            case Swap(_, _)              => "SWAP"
            case CP(_, _, _)             => "CP"
            case CRX(_, _, _)            => "CRX"
            case CRY(_, _, _)            => "CRY"
            case CRZ(_, _, _)            => "CRZ"
            case CU(_, _, _, _, _)       => "CU"
            case CCX(_, _, _)            => "CCX"
            case Measure(_)              => "MEASURE"
            case Reset(_)                => "RESET"
            case GPhase(_)               => "GPHASE"
            case NamedGate(name, _, _)   => name.toUpperCase
        }

    private def gateQubits(gate: Gate): Vector[Int] =
        gate match {
            case X(q)                         => Vector(q)
            case Y(q)                         => Vector(q)
            case Z(q)                         => Vector(q)
            case H(q)                         => Vector(q)
            case S(q)                         => Vector(q)
            case SDG(q)                       => Vector(q)
            case T(q)                         => Vector(q)
            case TDG(q)                       => Vector(q)
            case SX(q)                        => Vector(q)
            case SXDG(q)                      => Vector(q)
            case Id(q)                        => Vector(q)
            case Phase(_, q)                  => Vector(q)
            case RX(_, q)                     => Vector(q)
            case RY(_, q)                     => Vector(q)
            case RZ(_, q)                     => Vector(q)
            case U(_, _, _, q)                => Vector(q)
            case U2(_, _, q)                  => Vector(q)
            case U3(_, _, _, q)               => Vector(q)
            case CX(a, b)                     => Vector(a, b)
            case CY(a, b)                     => Vector(a, b)
            case CZ(a, b)                     => Vector(a, b)
            case CH(a, b)                     => Vector(a, b)
            case Swap(a, b)                   => Vector(a, b)
            case CP(a, _, b)                  => Vector(a, b)
            case CRX(a, _, b)                 => Vector(a, b)
            case CRY(a, _, b)                 => Vector(a, b)
            case CRZ(a, _, b)                 => Vector(a, b)
            case CU(a, _, _, _, b)            => Vector(a, b)
            case CCX(a, b, t)                 => Vector(a, b, t)
            case Measure(q)                   => Vector(q)
            case Reset(q)                     => Vector(q)
            case GPhase(_)                    => Vector.empty
            case NamedGate(_, _, qubits)      => qubits
        }

    private def remapGate(gate: Gate, remap: Map[Int, Int]): Option[Gate] = {
        def q(x: Int): Option[Int] = remap.get(x)
        def two(a: Int, b: Int)(f: (Int, Int) => Gate): Option[Gate] =
            (q(a), q(b)).mapN(f)

        gate match {
            case X(a)                         => q(a).map(X(_))
            case Y(a)                         => q(a).map(Y(_))
            case Z(a)                         => q(a).map(Z(_))
            case H(a)                         => q(a).map(H(_))
            case S(a)                         => q(a).map(S(_))
            case SDG(a)                       => q(a).map(SDG(_))
            case T(a)                         => q(a).map(T(_))
            case TDG(a)                       => q(a).map(TDG(_))
            case SX(a)                        => q(a).map(SX(_))
            case SXDG(a)                      => q(a).map(SXDG(_))
            case Id(a)                        => q(a).map(Id(_))
            case Phase(theta, a)              => q(a).map(Phase(theta, _))
            case RX(theta, a)                 => q(a).map(RX(theta, _))
            case RY(theta, a)                 => q(a).map(RY(theta, _))
            case RZ(theta, a)                 => q(a).map(RZ(theta, _))
            case U(theta, phi, lambda, a)     => q(a).map(U(theta, phi, lambda, _))
            case U2(phi, lambda, a)           => q(a).map(U2(phi, lambda, _))
            case U3(theta, phi, lambda, a)    => q(a).map(U3(theta, phi, lambda, _))
            case CX(a, b)                     => two(a, b)(CX.apply)
            case CY(a, b)                     => two(a, b)(CY.apply)
            case CZ(a, b)                     => two(a, b)(CZ.apply)
            case CH(a, b)                     => two(a, b)(CH.apply)
            case Swap(a, b)                   => two(a, b)(Swap.apply)
            case CP(a, theta, b)              => two(a, b)((x, y) => CP(x, theta, y))
            case CRX(a, theta, b)             => two(a, b)((x, y) => CRX(x, theta, y))
            case CRY(a, theta, b)             => two(a, b)((x, y) => CRY(x, theta, y))
            case CRZ(a, theta, b)             => two(a, b)((x, y) => CRZ(x, theta, y))
            case CU(a, theta, phi, lambda, b) => two(a, b)((x, y) => CU(x, theta, phi, lambda, y))
            case CCX(a, b, target)            => (q(a), q(b), q(target)).mapN(CCX.apply)
            case Measure(a)                   => q(a).map(Measure(_))
            case Reset(a)                     => q(a).map(Reset(_))
            case GPhase(theta)                => Some(GPhase(theta))
            case NamedGate(name, params, qs)  =>
                qs.toList.traverse(q).map(mapped => NamedGate(name, params, mapped.toVector))
        }
    }

    private implicit final class VectorDoubleOps(private val xs: Vector[Double]) extends AnyVal {
        def average: Option[Double] =
            if (xs.nonEmpty) Some(xs.sum / xs.size.toDouble) else None
    }
}
