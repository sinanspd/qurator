package qurator.testbed

import qurator.domain.calibration._
import qurator.domain.circuit.Circuit
import qurator.domain.circuit._
import qurator.domain.device.Device
import cats.syntax.all._
import cats.Applicative
import qurator.testbed.HaqaMapper.{DeviceTopology, PhysicalEdge}
import qurator.util.FidelityEstimator

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

final case class FakeCompiler[F[_] : Applicative](
    compiled: List[Tuple2[String, Map[String, Circuit]]],
    compileFallback: (Device, Circuit) => Circuit = (_: Device, circuit: Circuit) => circuit
) {
    private val compiledByCircuit: Map[String, Map[String, Circuit]] =
        compiled.reverse.toMap

    private val fallbackCache: TrieMap[(String, String, Int), Circuit] =
        TrieMap.empty

    def compileCircuitFor(device: Device, circuit: Circuit) = {
        val precompiled =
            compiledByCircuit
                .get(circuit.name)
                .flatMap(_.get(device.platformId))

        val out =
            precompiled.getOrElse {
                val key = (device.platformId, circuit.name, circuit.hashCode)
                fallbackCache.getOrElseUpdate(key, compileFallback(device, circuit))
            }

        out.pure[F]
    }
}

object FakeCompiler {

    final case class TopologyAwareConfig(
        placementSearchEnabled: Boolean = true,
        routeNonAdjacentGates: Boolean = true,
        distanceIndexEnabled: Boolean = true,
        maxLogicalSeedPairs: Int = 2,
        maxPhysicalEdgeSeeds: Int = 16,
        maxPhysicalNodeSeeds: Int = 16,
        maxPlacementCandidates: Int = 48,
        candidateNeighborExpansionRounds: Int = 1
    )

    object TopologyAwareConfig {
        val ultraFast: TopologyAwareConfig =
            TopologyAwareConfig(
                candidateNeighborExpansionRounds = 0
            )

        val fast: TopologyAwareConfig =
            TopologyAwareConfig()

        val balanced: TopologyAwareConfig =
            TopologyAwareConfig(
                maxLogicalSeedPairs = 3,
                maxPhysicalEdgeSeeds = 32,
                maxPhysicalNodeSeeds = 24,
                maxPlacementCandidates = 96,
                candidateNeighborExpansionRounds = 2
            )

        val quality: TopologyAwareConfig =
            TopologyAwareConfig(
                maxLogicalSeedPairs = 3,
                maxPhysicalEdgeSeeds = 48,
                maxPhysicalNodeSeeds = 48,
                maxPlacementCandidates = 0,
                candidateNeighborExpansionRounds = 2
            )
    }

    def topologyAware[F[_] : Applicative](
        registry: BenchmarkDeviceRegistry,
        compiled: List[Tuple2[String, Map[String, Circuit]]] = Nil,
        config: TopologyAwareConfig = TopologyAwareConfig.balanced
    ): FakeCompiler[F] = {
        val compiler = new TopologyAwareBenchmarkCompiler(registry, config)
        FakeCompiler[F](compiled = compiled, compileFallback = compiler.compile)
    }
}

private final case class TopologyAwareDeviceIndex(
    topology: DeviceTopology,
    calibration: CanonicalCalibration,
    supportedGateKinds: Set[String],
    nodeQuality: Map[Int, Double],
    edgeQuality: Map[PhysicalEdge, Double],
    rankedQubits: Vector[Int],
    rankedEdges: Vector[PhysicalEdge],
    distanceByPair: Map[(Int, Int), Int],
    routePathCache: TrieMap[(Int, Int), Option[Vector[Int]]]
)

private final class TopologyAwareBenchmarkCompiler(
    registry: BenchmarkDeviceRegistry,
    config: FakeCompiler.TopologyAwareConfig
) {

    private val indexCache: TrieMap[String, Option[TopologyAwareDeviceIndex]] =
        TrieMap.empty

    def compile(device: Device, circuit: Circuit): Circuit =
        if (circuit.qubits <= 0 || alreadyCompiledFor(device, circuit)) circuit
        else {
            indexFor(device) match {
                case Some(index) if index.topology.qubits.size >= logicalQubits(circuit).size =>
                    val mapping = choosePlacement(circuit, index)
                    routeCircuit(circuit, mapping, index).copy(
                        name = compiledName(circuit.name, device)
                    )
                case _ =>
                    circuit
            }
        }

    private def alreadyCompiledFor(device: Device, circuit: Circuit): Boolean =
        circuit.name == compiledName("", device) ||
            circuit.name.endsWith(s"_topology_${device.platformId}")

    private def compiledName(baseName: String, device: Device): String =
        if (baseName.nonEmpty) s"${baseName}_topology_${device.platformId}" else s"topology_${device.platformId}"

    private def indexFor(device: Device): Option[TopologyAwareDeviceIndex] =
        indexCache.getOrElseUpdate(device.platformId, buildIndex(device))

    private def buildIndex(device: Device): Option[TopologyAwareDeviceIndex] =
        registry.calibrationsById.get(device.platformId).flatMap { raw =>
            HaqaMapper.topologyFromCalibration(raw).map { topology =>
                val calibration = FidelityEstimator.normalizeCalibration(raw)
                val supportedGateKinds = effectiveGateKinds(device, raw, calibration)
                val nodeQuality =
                    topology.qubits.map(q => q -> qubitQuality(q, calibration)).toMap
                val edgeQuality =
                    topology.edges.map(e => e -> twoQubitQuality(e, calibration)).toMap
                val rankedQubits =
                    topology.qubits.sortBy(q => -nodeQuality.getOrElse(q, 0.0))
                val rankedEdges =
                    topology.edges.sortBy(e => -edgeQuality.getOrElse(e, 0.0))
                val distanceByPair =
                    if (config.distanceIndexEnabled) buildDistanceIndex(topology)
                    else Map.empty[(Int, Int), Int]

                TopologyAwareDeviceIndex(
                    topology = topology,
                    calibration = calibration,
                    supportedGateKinds = supportedGateKinds,
                    nodeQuality = nodeQuality,
                    edgeQuality = edgeQuality,
                    rankedQubits = rankedQubits,
                    rankedEdges = rankedEdges,
                    distanceByPair = distanceByPair,
                    routePathCache = TrieMap.empty
                )
            }
        }

    private def choosePlacement(circuit: Circuit, index: TopologyAwareDeviceIndex): Map[Int, Int] = {
        val logicals = logicalQubits(circuit)
        val interactions = logicalInteractions(circuit)
        val activity = logicalActivity(circuit)
        val logicalOrder =
            logicals.sortBy(q => (-(activity.getOrElse(q, 0.0) + interactionWeight(q, interactions)), q))

        val seeds = placementSeeds(logicalOrder, interactions, index)

        seeds
            .map(seed => completePlacement(seed, logicalOrder, interactions, activity, index))
            .maxByOption(mapping => placementScore(mapping, interactions, activity, index))
            .getOrElse {
                logicals
                    .zip(index.rankedQubits)
                    .toMap
            }
    }

    private def placementSeeds(
        logicalOrder: Vector[Int],
        interactions: Map[(Int, Int), Double],
        index: TopologyAwareDeviceIndex
    ): Vector[Map[Int, Int]] = {
        if (!config.placementSearchEnabled) {
            return logicalOrder.headOption
                .flatMap(logical => index.rankedQubits.headOption.map(physical => Map(logical -> physical)))
                .toVector
        }

        val topPhysicalEdges =
            index.rankedEdges
                .take(math.max(0, config.maxPhysicalEdgeSeeds))

        val topLogicalPairs =
            interactions.toVector
                .sortBy { case ((a, b), weight) => (-weight, a, b) }
                .take(math.max(0, config.maxLogicalSeedPairs))
                .map(_._1)

        val edgeSeeds =
            for {
                (logicalA, logicalB) <- topLogicalPairs
                edge <- topPhysicalEdges
                seed <- Vector(
                    Map(logicalA -> edge.u, logicalB -> edge.v),
                    Map(logicalA -> edge.v, logicalB -> edge.u)
                )
            } yield seed

        val firstLogical = logicalOrder.headOption.toVector
        val nodeSeeds =
            for {
                logical <- firstLogical
                physical <- index.rankedQubits.take(math.max(1, config.maxPhysicalNodeSeeds))
            } yield Map(logical -> physical)

        (edgeSeeds ++ nodeSeeds).distinct
    }

    private def completePlacement(
        seed: Map[Int, Int],
        logicalOrder: Vector[Int],
        interactions: Map[(Int, Int), Double],
        activity: Map[Int, Double],
        index: TopologyAwareDeviceIndex
    ): Map[Int, Int] = {
        val assigned = mutable.LinkedHashMap.empty[Int, Int] ++ seed
        val candidatePool = placementCandidatePool(seed, logicalOrder.size, index)
        val available = mutable.LinkedHashSet.empty[Int] ++ candidatePool.filterNot(seed.values.toSet)

        logicalOrder.foreach { logical =>
            if (!assigned.contains(logical)) {
                val best =
                    available.toVector.maxBy { physical =>
                        assignmentScore(logical, physical, assigned.toMap, interactions, activity, index)
                    }

                assigned += logical -> best
                available -= best
            }
        }

        assigned.toMap
    }

    private def assignmentScore(
        logical: Int,
        physical: Int,
        assigned: Map[Int, Int],
        interactions: Map[(Int, Int), Double],
        activity: Map[Int, Double],
        index: TopologyAwareDeviceIndex
    ): Double = {
        val node = index.nodeQuality.getOrElse(physical, 0.99)
        val localActivity = activity.getOrElse(logical, 0.0)
        val pair =
            assigned.iterator.map { case (otherLogical, otherPhysical) =>
                val weight = interactions.getOrElse(edgeKey(logical, otherLogical), 0.0)
                weight * physicalPairScore(physical, otherPhysical, index)
            }.sum

        node * (1.0 + 0.02 * localActivity) + 3.0 * pair
    }

    private def placementScore(
        mapping: Map[Int, Int],
        interactions: Map[(Int, Int), Double],
        activity: Map[Int, Double],
        index: TopologyAwareDeviceIndex
    ): Double = {
        val node =
            mapping.map { case (logical, physical) =>
                index.nodeQuality.getOrElse(physical, 0.99) * (1.0 + 0.02 * activity.getOrElse(logical, 0.0))
            }.sum

        val pair =
            interactions.map { case ((a, b), weight) =>
                for {
                    pa <- mapping.get(a)
                    pb <- mapping.get(b)
                } yield weight * physicalPairScore(pa, pb, index)
            }.flatten.sum

        node + 4.0 * pair
    }

    private def physicalPairScore(
        a: Int,
        b: Int,
        index: TopologyAwareDeviceIndex
    ): Double =
        index.topology.edge(a, b) match {
            case Some(edge) =>
                2.0 * index.edgeQuality.getOrElse(edge, 0.99)

            case None =>
                lookupDistance(a, b, index) match {
                    case Some(d) if d > 0 => 0.45 / d.toDouble
                    case Some(_) => 0.0
                    case None    => 0.0
                }
        }

    private def placementCandidatePool(
        seed: Map[Int, Int],
        logicalCount: Int,
        index: TopologyAwareDeviceIndex
    ): Vector[Int] = {
        if (config.maxPlacementCandidates <= 0) {
            index.topology.qubits
        } else {
            val anchors =
                seed.values.toVector.distinct
            val edgeEndpoints =
                index.rankedEdges
                    .take(math.max(1, config.maxPhysicalEdgeSeeds))
                    .flatMap(e => Vector(e.u, e.v))
                    .distinct
            val seedNeighborhood =
                expandNeighborhood(anchors, config.candidateNeighborExpansionRounds, index)
            val edgeNeighborhood =
                expandNeighborhood(edgeEndpoints.take(math.max(1, config.maxPhysicalEdgeSeeds)), math.max(0, config.candidateNeighborExpansionRounds - 1), index)

            val requested =
                math.max(logicalCount, config.maxPlacementCandidates)

            val candidateSet =
                (anchors ++
                    seedNeighborhood ++
                    edgeEndpoints ++
                    edgeNeighborhood ++
                    index.rankedQubits.take(math.max(requested, config.maxPhysicalNodeSeeds)) ++
                    index.rankedQubits)
                .distinct

            candidateSet
                .sortBy(q => -candidatePoolScore(q, anchors, index))
                .take(math.max(requested, seed.size + 1))
        }
    }

    private def expandNeighborhood(
        starts: Vector[Int],
        rounds: Int,
        index: TopologyAwareDeviceIndex
    ): Vector[Int] = {
        val seen = mutable.LinkedHashSet.empty[Int] ++ starts
        var frontier = starts.distinct
        var remaining = math.max(0, rounds)

        while (remaining > 0 && frontier.nonEmpty) {
            val next =
                frontier.flatMap(q => index.topology.neighbors.getOrElse(q, Vector.empty))
                    .filterNot(seen.contains)
                    .distinct
                    .sortBy(q => -index.nodeQuality.getOrElse(q, 0.0))

            seen ++= next
            frontier = next
            remaining -= 1
        }

        seen.toVector
    }

    private def candidatePoolScore(
        q: Int,
        anchors: Vector[Int],
        index: TopologyAwareDeviceIndex
    ): Double = {
        val node = index.nodeQuality.getOrElse(q, 0.99)
        val anchorScore =
            anchors.filterNot(_ == q).map { anchor =>
                index.topology.edge(q, anchor)
                    .map(edge => 0.65 * index.edgeQuality.getOrElse(edge, 0.99))
                    .orElse(lookupDistance(q, anchor, index).collect { case d if d > 0 => 0.25 / d.toDouble })
                    .getOrElse(0.0)
            }.maxOption.getOrElse(0.0)

        node + anchorScore
    }

    private def routeCircuit(
        circuit: Circuit,
        initialMapping: Map[Int, Int],
        index: TopologyAwareDeviceIndex
    ): Circuit = {
        val out = mutable.ListBuffer.empty[Gate]
        var logicalToPhysical = initialMapping

        def physicalOf(logical: Int): Int =
            logicalToPhysical.getOrElse(logical, logical)

        def applyLogicalSwap(a: Int, b: Int): Unit = {
            val pa = physicalOf(a)
            val pb = physicalOf(b)
            logicalToPhysical = logicalToPhysical.updated(a, pb).updated(b, pa)
        }

        circuit.remainingGates.foreach { gate =>
            val qs = gateQubits(gate).distinct
            gate match {
                case Swap(a, b) if qs.size == 2 =>
                    routeSwapGate(physicalOf(a), physicalOf(b), out, index)
                    applyLogicalSwap(a, b)

                case two if qs.size == 2 =>
                    val logicalA = qs(0)
                    val logicalB = qs(1)
                    routeTwoQubitGate(two, physicalOf(logicalA), physicalOf(logicalB), out, index)

                case one if qs.size == 1 =>
                    out ++= lowerSingleQubitGate(remapGate(one, physicalOf), index.supportedGateKinds)

                case other =>
                    out += remapGate(other, physicalOf)
            }
        }

        Circuit(
            remainingGates = out.toList,
            qubits = index.topology.maxPhysicalIndex + 1,
            name = circuit.name
        )
    }

    private def routeTwoQubitGate(
        gate: Gate,
        a: Int,
        b: Int,
        out: mutable.ListBuffer[Gate],
        index: TopologyAwareDeviceIndex
    ): Unit =
        if (index.topology.hasEdge(a, b)) {
            out ++= lowerAdjacentTwoQubitGate(gate, a, b, index.supportedGateKinds)
        } else {
            routePath(a, b, index) match {
                case Some(path) if path.size >= 2 =>
                    val inwardSwaps =
                        path.sliding(2).toVector.dropRight(1).collect { case Vector(x, y) => x -> y }
                    inwardSwaps.foreach { case (x, y) => out ++= lowerAdjacentSwap(x, y, index.supportedGateKinds) }
                    out ++= lowerAdjacentTwoQubitGate(gate, path(path.size - 2), b, index.supportedGateKinds)
                    inwardSwaps.reverse.foreach { case (x, y) => out ++= lowerAdjacentSwap(y, x, index.supportedGateKinds) }

                case _ =>
                    out ++= lowerAdjacentTwoQubitGate(gate, a, b, index.supportedGateKinds)
            }
        }

    private def routeSwapGate(
        a: Int,
        b: Int,
        out: mutable.ListBuffer[Gate],
        index: TopologyAwareDeviceIndex
    ): Unit =
        if (index.topology.hasEdge(a, b)) {
            out ++= lowerAdjacentSwap(a, b, index.supportedGateKinds)
        } else {
            routePath(a, b, index) match {
                case Some(path) if path.size >= 2 =>
                    val forward =
                        path.sliding(2).toVector.collect { case Vector(x, y) => x -> y }
                    val backward =
                        path.dropRight(1).sliding(2).toVector.reverse.collect { case Vector(x, y) => x -> y }
                    (forward ++ backward).foreach { case (x, y) =>
                        out ++= lowerAdjacentSwap(x, y, index.supportedGateKinds)
                    }

                case _ =>
                    out ++= lowerAdjacentSwap(a, b, index.supportedGateKinds)
            }
        }

    private def routingCost(index: TopologyAwareDeviceIndex): PhysicalEdge => Double = { edge =>
        1.0 / math.max(1e-6, index.edgeQuality.getOrElse(edge, 0.99))
    }

    private def routePath(
        a: Int,
        b: Int,
        index: TopologyAwareDeviceIndex
    ): Option[Vector[Int]] =
        if (!config.routeNonAdjacentGates) None
        else {
            index.routePathCache.getOrElseUpdate(
                edgeKey(a, b),
                index.topology.shortestPath(a, b, routingCost(index))
            )
        }

    private def lowerSingleQubitGate(gate: Gate, supportedGateKinds: Set[String]): List[Gate] =
        gate match {
            case Phase(theta, q) if !supports("phase", supportedGateKinds) && supports("rz", supportedGateKinds) =>
                List(RZ(theta, q))
            case other =>
                List(other)
        }

    private def lowerAdjacentTwoQubitGate(
        gate: Gate,
        a: Int,
        b: Int,
        supportedGateKinds: Set[String]
    ): List[Gate] =
        gate match {
            case CX(_, _) if supports("cx", supportedGateKinds) || supports("ecr", supportedGateKinds) => List(CX(a, b))
            case CX(_, _) if supports("cz", supportedGateKinds) => List(H(b), CZ(a, b), H(b))
            case CZ(_, _) if supports("cz", supportedGateKinds) => List(CZ(a, b))
            case CZ(_, _) if supports("cx", supportedGateKinds) || supports("ecr", supportedGateKinds) => List(H(b), CX(a, b), H(b))
            case CY(_, _) if supports("cx", supportedGateKinds) || supports("ecr", supportedGateKinds) => List(SDG(b), CX(a, b), S(b))
            case CY(_, _) if supports("cz", supportedGateKinds) => List(SDG(b), H(b), CZ(a, b), H(b), S(b))
            case Swap(_, _) => lowerAdjacentSwap(a, b, supportedGateKinds)
            case CP(_, theta, _) if supports("cp", supportedGateKinds) => List(CP(a, theta, b))
            case CRX(_, theta, _) if supports("crx", supportedGateKinds) => List(CRX(a, theta, b))
            case CRY(_, theta, _) if supports("cry", supportedGateKinds) => List(CRY(a, theta, b))
            case CRZ(_, theta, _) => List(CRZ(a, theta, b))
            case CH(_, _) if supports("ch", supportedGateKinds) => List(CH(a, b))
            case CU(_, theta, phi, lambda, _) if supports("cu", supportedGateKinds) => List(CU(a, theta, phi, lambda, b))
            case NamedGate(name, params, _) if name.equalsIgnoreCase("rzz") =>
                List(CRZ(a, params.headOption.getOrElse("0"), b))
            case NamedGate(name, params, _) =>
                List(NamedGate(name, params, Vector(a, b)))
            case other =>
                remapTwoQubitGate(other, a, b) :: Nil
        }

    private def lowerAdjacentSwap(
        a: Int,
        b: Int,
        supportedGateKinds: Set[String]
    ): List[Gate] =
        if (supports("swap", supportedGateKinds)) List(Swap(a, b))
        else if (supports("cx", supportedGateKinds) || supports("ecr", supportedGateKinds)) List(CX(a, b), CX(b, a), CX(a, b))
        else if (supports("cz", supportedGateKinds))
            List(H(b), CZ(a, b), H(b), H(a), CZ(a, b), H(a), H(b), CZ(a, b), H(b))
        else List(Swap(a, b))

    private def effectiveGateKinds(
        device: Device,
        raw: DeviceCalibration,
        calibration: CanonicalCalibration
    ): Set[String] = {
        val deviceKinds = device.gateSet.map(gateKind)
        val basisKinds =
            raw match {
                case ibm: IBMCalibration => ibm.basisGates.map(_.toLowerCase)
                case _                   => Nil
            }
        val canonicalKinds =
            (calibration.eps1q.keys.map(_._2.toLowerCase) ++ calibration.eps2q.keys.map(_._2.toLowerCase)).toList

        val nativeKinds = deviceKinds ++ basisKinds
        if (nativeKinds.nonEmpty) nativeKinds.toSet else canonicalKinds.toSet
    }

    private def buildDistanceIndex(topology: DeviceTopology): Map[(Int, Int), Int] =
        topology.qubits.flatMap { start =>
            val distances = mutable.Map.empty[Int, Int]
            val queue = mutable.Queue.empty[Int]

            distances += start -> 0
            queue.enqueue(start)

            while (queue.nonEmpty) {
                val current = queue.dequeue()
                val nextDistance = distances(current) + 1

                topology.neighbors.getOrElse(current, Vector.empty).foreach { neighbor =>
                    if (!distances.contains(neighbor)) {
                        distances += neighbor -> nextDistance
                        queue.enqueue(neighbor)
                    }
                }
            }

            distances.iterator.map { case (end, distance) =>
                edgeKey(start, end) -> distance
            }.toVector
        }.toMap

    private def lookupDistance(
        a: Int,
        b: Int,
        index: TopologyAwareDeviceIndex
    ): Option[Int] =
        index.distanceByPair.get(edgeKey(a, b)).orElse {
            if (config.distanceIndexEnabled) None
            else index.topology.shortestPath(a, b, _ => 1.0).map(path => math.max(1, path.size - 1))
        }

    private def qubitQuality(q: Int, calibration: CanonicalCalibration): Double = {
        val oneQErrors =
            List("X", "SX", "RX", "H")
                .flatMap(g => calibration.eps1q.get((q, g)))
                .filter(_.isFinite)
        val oneQFidelity =
            oneQErrors.headOption
                .map(_ => 1.0 - oneQErrors.sum / oneQErrors.size.toDouble)
                .orElse(calibration.eps1qAvg.map(e => 1.0 - e))
                .getOrElse(0.999)
        val readout = calibration.readoutFidFor(q)

        clamp01(0.65 * oneQFidelity + 0.35 * readout)
    }

    private def twoQubitQuality(edge: PhysicalEdge, calibration: CanonicalCalibration): Double = {
        val key = edgeKey(edge.u, edge.v)
        val errors =
            List("CX", "CZ", "ECR", "RZZ", "CRotate")
                .flatMap(g => calibration.eps2q.get((key, g)))
                .filter(_.isFinite)
        val error =
            errors.minOption
                .orElse(calibration.eps2qAvg)
                .getOrElse(0.01)

        clamp01(1.0 - error)
    }

    private def logicalQubits(circuit: Circuit): Vector[Int] =
        ((0 until circuit.qubits).toVector ++ circuit.remainingGates.flatMap(gateQubits)).distinct.sorted

    private def logicalInteractions(circuit: Circuit): Map[(Int, Int), Double] = {
        val weights = mutable.Map.empty[(Int, Int), Double].withDefaultValue(0.0)
        circuit.remainingGates.foreach { gate =>
            val qs = gateQubits(gate).distinct
            if (qs.size >= 2) {
                qs.combinations(2).foreach {
                    case Vector(a, b) => weights(edgeKey(a, b)) += 1.0
                    case _            => ()
                }
            }
        }
        weights.toMap
    }

    private def logicalActivity(circuit: Circuit): Map[Int, Double] = {
        val weights = mutable.Map.empty[Int, Double].withDefaultValue(0.0)
        circuit.remainingGates.foreach { gate =>
            val qs = gateQubits(gate).distinct
            val increment = if (qs.size >= 2) 1.0 else 0.25
            qs.foreach(q => weights(q) += increment)
        }
        weights.toMap
    }

    private def interactionWeight(q: Int, interactions: Map[(Int, Int), Double]): Double =
        interactions.collect { case ((a, b), weight) if a == q || b == q => weight }.sum

    private def supports(kind: String, supportedGateKinds: Set[String]): Boolean =
        supportedGateKinds.isEmpty || supportedGateKinds.contains(kind)

    private def remapGate(gate: Gate, mapQ: Int => Int): Gate =
        gate match {
            case X(q) => X(mapQ(q))
            case Y(q) => Y(mapQ(q))
            case Z(q) => Z(mapQ(q))
            case H(q) => H(mapQ(q))
            case S(q) => S(mapQ(q))
            case SDG(q) => SDG(mapQ(q))
            case T(q) => T(mapQ(q))
            case TDG(q) => TDG(mapQ(q))
            case SX(q) => SX(mapQ(q))
            case SXDG(q) => SXDG(mapQ(q))
            case Id(q) => Id(mapQ(q))
            case Phase(theta, q) => Phase(theta, mapQ(q))
            case RX(theta, q) => RX(theta, mapQ(q))
            case RY(theta, q) => RY(theta, mapQ(q))
            case RZ(theta, q) => RZ(theta, mapQ(q))
            case U(theta, phi, lambda, q) => U(theta, phi, lambda, mapQ(q))
            case U2(phi, lambda, q) => U2(phi, lambda, mapQ(q))
            case U3(theta, phi, lambda, q) => U3(theta, phi, lambda, mapQ(q))
            case CX(a, b) => CX(mapQ(a), mapQ(b))
            case CY(a, b) => CY(mapQ(a), mapQ(b))
            case CZ(a, b) => CZ(mapQ(a), mapQ(b))
            case CH(a, b) => CH(mapQ(a), mapQ(b))
            case Swap(a, b) => Swap(mapQ(a), mapQ(b))
            case CP(a, theta, b) => CP(mapQ(a), theta, mapQ(b))
            case CRX(a, theta, b) => CRX(mapQ(a), theta, mapQ(b))
            case CRY(a, theta, b) => CRY(mapQ(a), theta, mapQ(b))
            case CRZ(a, theta, b) => CRZ(mapQ(a), theta, mapQ(b))
            case CU(a, theta, phi, lambda, b) => CU(mapQ(a), theta, phi, lambda, mapQ(b))
            case CCX(a, b, c) => CCX(mapQ(a), mapQ(b), mapQ(c))
            case Measure(q) => Measure(mapQ(q))
            case Reset(q) => Reset(mapQ(q))
            case GPhase(theta) => GPhase(theta)
            case NamedGate(name, params, qubits) => NamedGate(name, params, qubits.map(mapQ))
        }
    
    private def remapTwoQubitGate(gate: Gate, a: Int, b: Int): Gate =
        gate match {
            case CX(_, _) => CX(a, b)
            case CY(_, _) => CY(a, b)
            case CZ(_, _) => CZ(a, b)
            case CH(_, _) => CH(a, b)
            case Swap(_, _) => Swap(a, b)
            case CP(_, theta, _) => CP(a, theta, b)
            case CRX(_, theta, _) => CRX(a, theta, b)
            case CRY(_, theta, _) => CRY(a, theta, b)
            case CRZ(_, theta, _) => CRZ(a, theta, b)
            case CU(_, theta, phi, lambda, _) => CU(a, theta, phi, lambda, b)
            case NamedGate(name, params, _) => NamedGate(name, params, Vector(a, b))
            case other => other
        }

    private def gateQubits(gate: Gate): Vector[Int] =
        gate match {
            case X(q) => Vector(q)
            case Y(q) => Vector(q)
            case Z(q) => Vector(q)
            case H(q) => Vector(q)
            case S(q) => Vector(q)
            case SDG(q) => Vector(q)
            case T(q) => Vector(q)
            case TDG(q) => Vector(q)
            case SX(q) => Vector(q)
            case SXDG(q) => Vector(q)
            case Id(q) => Vector(q)
            case Phase(_, q) => Vector(q)
            case RX(_, q) => Vector(q)
            case RY(_, q) => Vector(q)
            case RZ(_, q) => Vector(q)
            case U(_, _, _, q) => Vector(q)
            case U2(_, _, q) => Vector(q)
            case U3(_, _, _, q) => Vector(q)
            case CX(a, b) => Vector(a, b)
            case CY(a, b) => Vector(a, b)
            case CZ(a, b) => Vector(a, b)
            case CH(a, b) => Vector(a, b)
            case Swap(a, b) => Vector(a, b)
            case CP(a, _, b) => Vector(a, b)
            case CRX(a, _, b) => Vector(a, b)
            case CRY(a, _, b) => Vector(a, b)
            case CRZ(a, _, b) => Vector(a, b)
            case CU(a, _, _, _, b) => Vector(a, b)
            case CCX(a, b, c) => Vector(a, b, c)
            case Measure(q) => Vector(q)
            case Reset(q) => Vector(q)
            case GPhase(_) => Vector.empty
            case NamedGate(_, _, qubits) => qubits
        }

    private def gateKind(gate: Gate): String =
        gate match {
            case X(_) => "x"
            case Y(_) => "y"
            case Z(_) => "z"
            case H(_) => "h"
            case S(_) => "s"
            case SDG(_) => "sdg"
            case T(_) => "t"
            case TDG(_) => "tdg"
            case SX(_) => "sx"
            case SXDG(_) => "sxdg"
            case Id(_) => "id"
            case Phase(_, _) => "phase"
            case RX(_, _) => "rx"
            case RY(_, _) => "ry"
            case RZ(_, _) => "rz"
            case U(_, _, _, _) => "u"
            case U2(_, _, _) => "u2"
            case U3(_, _, _, _) => "u3"
            case CX(_, _) => "cx"
            case CY(_, _) => "cy"
            case CZ(_, _) => "cz"
            case CH(_, _) => "ch"
            case Swap(_, _) => "swap"
            case CP(_, _, _) => "cp"
            case CRX(_, _, _) => "crx"
            case CRY(_, _, _) => "cry"
            case CRZ(_, _, _) => "crz"
            case CU(_, _, _, _, _) => "cu"
            case CCX(_, _, _) => "ccx"
            case Measure(_) => "measure"
            case Reset(_) => "reset"
            case GPhase(_) => "gphase"
            case NamedGate(name, _, _) => name.toLowerCase
        }

    private def edgeKey(a: Int, b: Int): (Int, Int) =
        if (a <= b) (a, b) else (b, a)

    private def clamp01(value: Double): Double =
        math.max(0.0, math.min(1.0, value))
}
