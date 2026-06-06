package qurator.testbed

import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.util.FidelityEstimator

import scala.annotation.tailrec
import scala.collection.mutable

//rough estimation. Without a full solver, this results in a larger number of SWAPS
object HaqaMapper {

  final case class GateStats(
      errorRate: Double,
      durationNs: Long
  ) {
    def fidelity: Double = clamp01(1.0 - errorRate)
  }

  final case class QubitCalibration(
      t1Seconds: Option[Double] = None,
      t2Seconds: Option[Double] = None,
      readoutError: Double = 0.0,
      gateStats: Map[String, GateStats] = Map.empty
  ) {
    def readoutFidelity: Double = clamp01(1.0 - readoutError)
  }

  final case class EdgeCalibration(
      gateStats: Map[String, GateStats] = Map.empty
  ) {
    def bestFidelity(preferredGateKinds: Set[String]): Double = {
      val candidates =
        if (preferredGateKinds.nonEmpty) {
          preferredGateKinds.toVector.flatMap(g => gateStats.get(g).map(_.fidelity))
        } else gateStats.valuesIterator.map(_.fidelity).toVector

      if (candidates.nonEmpty) candidates.max else 0.99
    }

    def bestError(preferredGateKinds: Set[String]): Double = {
      val candidates =
        if (preferredGateKinds.nonEmpty) {
          preferredGateKinds.toVector.flatMap(g => gateStats.get(g).map(_.errorRate))
        } else gateStats.valuesIterator.map(_.errorRate).toVector

      if (candidates.nonEmpty) candidates.min else 0.01
    }

    def bestGate(preferredGateKinds: Set[String]): Option[String] = {
      val candidates =
        if (preferredGateKinds.nonEmpty) {
          preferredGateKinds.toVector.flatMap(g => gateStats.get(g).map(gs => g -> gs.errorRate))
        } else gateStats.toVector.map { case (g, gs) => g -> gs.errorRate }

      if (candidates.isEmpty) None else Some(candidates.minBy(_._2)._1)
    }
  }

  final case class PhysicalEdge private (u: Int, v: Int) {
    def contains(q: Int): Boolean = u == q || v == q
    def other(q: Int): Int = if (u == q) v else if (v == q) u else throw new IllegalArgumentException(s"$q not in edge $this")
  }

  object PhysicalEdge {
    def apply(a: Int, b: Int): PhysicalEdge = {
      require(a != b, s"Self-loops are not allowed: ($a, $b)")
      if (a < b) new PhysicalEdge(a, b) else new PhysicalEdge(b, a)
    }
  }

  final case class DeviceTopology(
      qubits: Vector[Int],
      edges: Vector[PhysicalEdge]
  ) {
    require(qubits.distinct.size == qubits.size, "DeviceTopology.qubits contains duplicates")
    private val qubitSet = qubits.toSet
    require(edges.forall(e => qubitSet.contains(e.u) && qubitSet.contains(e.v)), "All edge endpoints must be present in topology.qubits")

    lazy val edgeSet: Set[PhysicalEdge] = edges.toSet

    lazy val neighbors: Map[Int, Vector[Int]] = {
      val init = qubits.map(_ -> Vector.empty[Int]).toMap
      val built = edges.foldLeft(init) {
        case (acc, PhysicalEdge(u, v)) =>
          acc.updated(u, acc(u) :+ v).updated(v, acc(v) :+ u)
      }
      built.map { case (q, ns) => q -> ns.distinct.sorted }
    }

    lazy val degree: Map[Int, Int] = neighbors.map { case (q, ns) => q -> ns.size }

    val maxPhysicalIndex: Int = if (qubits.isEmpty) 0 else qubits.max

    def hasEdge(a: Int, b: Int): Boolean = edgeSet.contains(PhysicalEdge(a, b))

    def edge(a: Int, b: Int): Option[PhysicalEdge] = {
      val e = PhysicalEdge(a, b)
      if (edgeSet.contains(e)) Some(e) else None
    }

    def inducedSubgraph(nodes: Set[Int]): DeviceTopology = {
      DeviceTopology(
        qubits = qubits.filter(nodes.contains),
        edges = edges.filter(e => nodes.contains(e.u) && nodes.contains(e.v))
      )
    }

    def shortestPath(start: Int, goal: Int, edgeCost: PhysicalEdge => Double): Option[Vector[Int]] = {
      if (start == goal) return Some(Vector(start))
      if (!qubitSet.contains(start) || !qubitSet.contains(goal)) return None

      val dist = mutable.Map.empty[Int, Double].withDefaultValue(Double.PositiveInfinity)
      val prev = mutable.Map.empty[Int, Int]
      implicit val ord: Ordering[(Double, Int)] = Ordering.by[(Double, Int), Double](_._1).reverse
      val pq = mutable.PriorityQueue.empty[(Double, Int)]

      dist(start) = 0.0
      pq.enqueue((0.0, start))

      while (pq.nonEmpty) {
        val (negD, current) = pq.dequeue()
        val currentDist = -negD
        if (current == goal) {
          return Some(reconstructPath(prev.toMap, start, goal))
        }
        if (currentDist <= dist(current) + 1e-12) {
          neighbors.getOrElse(current, Vector.empty).foreach { n =>
            val e = PhysicalEdge(current, n)
            val alt = dist(current) + edgeCost(e)
            if (alt + 1e-12 < dist(n)) {
              dist(n) = alt
              prev(n) = current
              pq.enqueue((-alt, n))
            }
          }
        }
      }

      None
    }

    private def reconstructPath(prev: Map[Int, Int], start: Int, goal: Int): Vector[Int] = {
      val out = mutable.ArrayBuffer[Int](goal)
      var cur = goal
      while (cur != start) {
        cur = prev(cur)
        out += cur
      }
      out.reverse.toVector
    }
  }

  object DeviceTopology {
    def fromEdges(edges: Iterable[(Int, Int)], qubits: Iterable[Int] = Nil): DeviceTopology = {
      val edgeObjs = edges.iterator.map { case (a, b) => PhysicalEdge(a, b) }.toVector.distinct
      val qs = (qubits.toVector ++ edgeObjs.flatMap(e => Vector(e.u, e.v))).distinct.sorted
      DeviceTopology(qs, edgeObjs)
    }
  }

  final case class GeneralDeviceCalibration(
      qubitCalibration: Map[Int, QubitCalibration],
      edgeCalibration: Map[PhysicalEdge, EdgeCalibration],
      defaultSingleQubitGateStats: Map[String, GateStats] = Map.empty,
      defaultTwoQubitGateStats: Map[String, GateStats] = Map.empty,
      defaultReadoutError: Double = 0.0
  ) {
    def qubit(q: Int): QubitCalibration =
      qubitCalibration.getOrElse(q, QubitCalibration(readoutError = defaultReadoutError))

    def edge(e: PhysicalEdge): EdgeCalibration =
      edgeCalibration.getOrElse(e, EdgeCalibration(defaultTwoQubitGateStats))

    def singleQubitFidelity(q: Int, gateKind: String): Double = {
      val stat = qubit(q).gateStats.get(gateKind).orElse(defaultSingleQubitGateStats.get(gateKind))
      stat.map(_.fidelity).getOrElse(0.999)
    }

    def readoutFidelity(q: Int): Double = qubit(q).readoutFidelity

    def singleQubitStats(q: Int, gateKind: String): GateStats = {
      val qc = qubit(q)
      val aliases = singleGateAliases(gateKind)
      aliases.iterator
        .flatMap(k => qc.gateStats.get(k).orElse(defaultSingleQubitGateStats.get(k)))
        .toSeq
        .headOption
        .getOrElse {
          if (Set("rz", "phase", "p", "s", "sdg", "t", "tdg").contains(gateKind.toLowerCase)) GateStats(0.0, 0L)
          else GateStats(0.001, 0L)
        }
    }

    def preferredTwoQubitGate(e: PhysicalEdge, preferredGateKinds: Set[String]): (String, GateStats) = {
      val ec = edge(e)
      val preferred = if (preferredGateKinds.nonEmpty) preferredGateKinds.toVector else ec.gateStats.keys.toVector
      preferred.iterator
        .flatMap(g => ec.gateStats.get(g).map(gs => g -> gs))
        .toSeq
        .sortBy(_._2.errorRate)
        .headOption
        .orElse(ec.gateStats.toSeq.sortBy(_._2.errorRate).headOption)
        .orElse(defaultTwoQubitGateStats.toSeq.sortBy(_._2.errorRate).headOption)
        .getOrElse("cx" -> GateStats(0.01, 0L))
    }
  }

  final case class QuantumDevice(
      gateSet: List[Gate],
      topology: DeviceTopology
  ) {
    lazy val supportedGateKinds: Set[String] = gateSet.iterator.map(gateKind).toSet
    lazy val supportedTwoQubitGateKinds: Set[String] = supportedGateKinds.intersect(TwoQubitGateKinds)
  }

  final case class Config(
      regionRewardWeight: Double = 0.35,
      expansionRounds: Int = 1,
      strictGateSetCheck: Boolean = false,
      qubitScoreWeight: Double = 1.0,
      pairScoreWeight: Double = 3.0,
      moveImportancePenaltyWeight: Double = 0.20,
      swapLengthWeight: Double = 0.05,
      placementEdgeWeight: Double = 0.60,
      placementCoherenceWeight: Double = 0.15,
      placementReadoutWeight: Double = 0.25,
      routingTwoQErrorWeight: Double = 1.00,
      routingSingleQErrorWeight: Double = 0.12,
      routingCoherenceWeight: Double = 0.20
  )

  def mapCircuit(
      circuit: Circuit,
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      config: Config = Config()
  ): Circuit = {
    require(circuit.qubits > 0, "Circuit must contain at least one logical qubit")
    require(device.topology.qubits.nonEmpty, "Device topology must contain at least one physical qubit")
    require(device.topology.qubits.size >= circuit.qubits, s"Device has only ${device.topology.qubits.size} physical qubits but circuit needs ${circuit.qubits}")

    val normalized = circuit.copy(remainingGates = circuit.remainingGates.flatMap(normalizeInputGate))
    val regions = recursiveCommunityFusion(device, calibration, config.regionRewardWeight)
    val chosenRegion = chooseExpandedRegion(normalized, device, regions, config.expansionRounds)
    val initialPlacement = fidelityAwareInitialPlacement(normalized, device, calibration, chosenRegion, config)
    val routed = routeCircuit(normalized, device, calibration, chosenRegion, initialPlacement, config)

    routed.copy(
      qubits = device.topology.maxPhysicalIndex + 1,
      name = if (circuit.name.nonEmpty) s"${circuit.name}_haqa" else "haqa_mapped"
    )
  }

  /**
    * Helper that matches the IBMCalibration shape from the chat without depending
    * on the original class/package.
    */
  def fromIbmLikeCalibration(
      qubits: List[Int],
      edges: List[(Int, Int)],
      t1Seconds: List[(Int, Double)],
      t2Seconds: List[(Int, Double)],
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
      rzzGateLengthNs: Long
  ): GeneralDeviceCalibration = {
    val t1Map = t1Seconds.toMap
    val t2Map = t2Seconds.toMap
    val roErr = (probMeasu0Presp1 + probMeasu1Presp0) / 2.0

    val qCal = qubits.map { q =>
      q -> QubitCalibration(
        t1Seconds = t1Map.get(q),
        t2Seconds = t2Map.get(q),
        readoutError = roErr,
        gateStats = Map(
          "id" -> GateStats(idError, idLengthNs),
          "rx" -> GateStats(rxError, singleQGateLengthNs),
          "x"  -> GateStats(pauliXError, singleQGateLengthNs)
        )
      )
    }.toMap

    val eCal = edges.map {
      case (a, b) =>
        PhysicalEdge(a, b) -> EdgeCalibration(
          gateStats = Map(
            "cz" -> GateStats(czError, czGateLengthNs),
            "rzz" -> GateStats(rzzError, rzzGateLengthNs),
            "cx" -> GateStats(czError, twoQGateLengthNs)
          )
        )
    }.toMap

    GeneralDeviceCalibration(
      qubitCalibration = qCal,
      edgeCalibration = eCal,
      defaultSingleQubitGateStats = Map(
        "id" -> GateStats(idError, idLengthNs),
        "rx" -> GateStats(rxError, singleQGateLengthNs),
        "x"  -> GateStats(pauliXError, singleQGateLengthNs)
      ),
      defaultTwoQubitGateStats = Map(
        "cz" -> GateStats(czError, czGateLengthNs),
        "rzz" -> GateStats(rzzError, rzzGateLengthNs),
        "cx" -> GateStats(czError, twoQGateLengthNs)
      ),
      defaultReadoutError = roErr
    )
  }

  def topologyFromCalibration(calibration: DeviceCalibration): Option[DeviceTopology] =
    topologyOf(calibration).map(t => DeviceTopology.fromEdges(t.normalizedEdges, t.qubits))

  def fromCalibration(calibration: DeviceCalibration): Option[GeneralDeviceCalibration] =
    topologyFromCalibration(calibration).map { topology =>
      val canonical = FidelityEstimator.normalizeCalibration(calibration)

      val qubitCalibration =
        topology.qubits.map { q =>
          val gateStats =
            canonical.eps1q.collect {
              case ((qq, gate), errorRate) if qq == q =>
                gate.toLowerCase -> GateStats(
                  errorRate = errorRate,
                  durationNs = canonical.dur1qNs.getOrElse(gate, canonical.dur1qAvgNs.getOrElse(0L))
                )
            }

          val readoutError =
            1.0 - canonical.readoutFidelity.get(q).orElse(canonical.readoutFidelityAvg).getOrElse(1.0)

          q -> QubitCalibration(
            t1Seconds = canonical.t1.get(q).orElse(canonical.t1Avg),
            t2Seconds = canonical.t2.get(q).orElse(canonical.t2Avg),
            readoutError = clamp01(readoutError),
            gateStats = gateStats
          )
        }.toMap

      val edgeCalibration =
        topology.edges.map { edge =>
          val key = (edge.u, edge.v)
          val gateStats =
            canonical.eps2q.collect {
              case ((pair, gate), errorRate) if pair == key =>
                gate.toLowerCase -> GateStats(
                  errorRate = errorRate,
                  durationNs = canonical.dur2qNs.getOrElse(gate, canonical.dur2qAvgNs.getOrElse(0L))
                )
            }

          edge -> EdgeCalibration(gateStats)
        }.toMap

      val defaultSingle =
        canonical.dur1qNs.keySet.map { gate =>
          gate.toLowerCase -> GateStats(
            errorRate = canonical.eps1qAvg.getOrElse(0.001),
            durationNs = canonical.dur1qNs.getOrElse(gate, canonical.dur1qAvgNs.getOrElse(0L))
          )
        }.toMap

      val defaultTwo =
        canonical.dur2qNs.keySet.map { gate =>
          gate.toLowerCase -> GateStats(
            errorRate = canonical.eps2qAvg.getOrElse(0.01),
            durationNs = canonical.dur2qNs.getOrElse(gate, canonical.dur2qAvgNs.getOrElse(0L))
          )
        }.toMap

      GeneralDeviceCalibration(
        qubitCalibration = qubitCalibration,
        edgeCalibration = edgeCalibration,
        defaultSingleQubitGateStats = defaultSingle,
        defaultTwoQubitGateStats = defaultTwo,
        defaultReadoutError = canonical.readoutFidelityAvg.map(fid => clamp01(1.0 - fid)).getOrElse(0.0)
      )
    }

  private final case class Region(
      nodes: Set[Int],
      edges: Set[PhysicalEdge],
      modularity: Double,
      avgInternalFidelity: Double,
      reward: Double
  ) {
    val size: Int = nodes.size
  }

  private def recursiveCommunityFusion(
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      omega: Double
  ): Vector[Region] = {
    val topo = device.topology
    var communities = topo.qubits.map(q => Set(q)).toVector
    val collected = mutable.LinkedHashMap.empty[Set[Int], Region]

    while (communities.size > 1) {
      val pairs = for {
        i <- communities.indices
        j <- (i + 1) until communities.size
      } yield (i, j)

      val scored = pairs.flatMap {
        case (i, j) =>
          val a = communities(i)
          val b = communities(j)
          val cross = crossingEdges(topo, a, b)
          if (cross.isEmpty) None
          else {
            val merged = a ++ b
            val partition = communities.patch(i, Nil, 1).patch(j - 1, Nil, 1) :+ merged
            val q = modularity(topo, partition)
            val avgF = averageFidelity(cross.toVector, device, calibration)
            val reward = q + omega * avgF
            Some((i, j, reward, q, avgF, merged))
          }
      }

      if (scored.isEmpty) {
        val a = communities.head
        val b = communities(1)
        val merged = a ++ b
        communities = communities.drop(2) :+ merged
      } else {
        val (bestI, bestJ, reward, q, avgF, merged) = scored.maxBy(_._3)
        val internalEdges = topo.edges.filter(e => merged.contains(e.u) && merged.contains(e.v)).toSet
        collected.getOrElseUpdate(
          merged,
          Region(merged, internalEdges, q, avgF, reward)
        )

        communities = communities.patch(bestI, Nil, 1).patch(bestJ - 1, Nil, 1) :+ merged
      }
    }

    if (collected.isEmpty) {
      Vector(
        Region(
          nodes = topo.qubits.toSet,
          edges = topo.edges.toSet,
          modularity = 0.0,
          avgInternalFidelity = averageFidelity(topo.edges, device, calibration),
          reward = 0.0
        )
      )
    } else collected.values.toVector.sortBy(r => (r.size, -r.reward))
  }

  private def chooseExpandedRegion(
      circuit: Circuit,
      device: QuantumDevice,
      regions: Vector[Region],
      expansionRounds: Int
  ): DeviceTopology = {
    val nLogical = circuit.qubits
    val base =
      regions.filter(_.size >= nLogical).sortBy(r => (r.size, -r.avgInternalFidelity, -r.reward)).headOption
        .getOrElse(Region(device.topology.qubits.toSet, device.topology.edges.toSet, 0.0, 0.0, 0.0))

    var nodes = base.nodes
    var rounds = 0
    while (rounds < expansionRounds) {
      val boundary = nodes.flatMap(q => device.topology.neighbors.getOrElse(q, Vector.empty)).diff(nodes)
      nodes = nodes ++ boundary
      rounds += 1
    }

    device.topology.inducedSubgraph(nodes)
  }

  private def fidelityAwareInitialPlacement(
      circuit: Circuit,
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      region: DeviceTopology,
      config: Config
  ): Map[Int, Int] = {
    val interaction = buildInteractionGraph(circuit)
    val logicalImportance = logicalQubitImportance(circuit)
    val measured = measuredLogicalQubits(circuit)
    val basePhysicalScore = physicalQubitScores(region, device, calibration, config)

    val logicalOrder = (0 until circuit.qubits).toVector.sortBy { q =>
      val measureBoost = if (measured.contains(q)) 0.5 else 0.0
      (-(logicalImportance.getOrElse(q, 0.0) + measureBoost), q)
    }
    val unassignedPhysical = mutable.Set.empty[Int] ++ region.qubits
    val assigned = mutable.LinkedHashMap.empty[Int, Int]

    logicalOrder.foreach { logical =>
      val bestPhysical = unassignedPhysical.toVector.maxBy { physical =>
        val measuredBonus = if (measured.contains(logical)) config.placementReadoutWeight * calibration.readoutFidelity(physical) else 0.0
        val nodeScore = config.qubitScoreWeight * (basePhysicalScore.getOrElse(physical, 0.0) + measuredBonus)
        val pairScore = assigned.iterator.map {
          case (otherLogical, otherPhysical) =>
            val weight = interaction.getOrElse(logicalEdge(logical, otherLogical), 0.0)
            if (weight <= 0.0) 0.0
            else {
              val adjacencyScore = region.edge(physical, otherPhysical)
                .map { e =>
                  val (_, gs) = calibration.preferredTwoQubitGate(e, device.supportedTwoQubitGateKinds)
                  val base = safeNegLogFidelity(gs.fidelity)
                  1.0 / (1.0 + base)
                }
                .getOrElse {
                  shortestDistance(region, physical, otherPhysical).map(d => 1.0 / (1.0 + d.toDouble)).getOrElse(0.0)
                }
              weight * adjacencyScore
            }
        }.sum
        nodeScore + config.pairScoreWeight * pairScore
      }
      assigned += logical -> bestPhysical
      unassignedPhysical -= bestPhysical
    }

    assigned.toMap
  }

  private final case class MappingState(
      logicalToPhysical: Map[Int, Int],
      physicalToLogical: Map[Int, Int]
  ) {
    def physicalOf(logical: Int): Int = logicalToPhysical(logical)

    def swap(a: Int, b: Int): MappingState = {
      val lA = physicalToLogical.get(a)
      val lB = physicalToLogical.get(b)

      val newL2P = logicalToPhysical ++ lA.map(_ -> b) ++ lB.map(_ -> a)
      val removed = physicalToLogical -- Seq(a, b)
      val newP2L = removed ++ lA.map(b -> _) ++ lB.map(a -> _)

      MappingState(newL2P, newP2L)
    }
  }

  private def routeCircuit(
      circuit: Circuit,
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      region: DeviceTopology,
      initialPlacement: Map[Int, Int],
      config: Config
  ): Circuit = {
    val emitted = mutable.ListBuffer.empty[Gate]
    val logicalImportance = logicalQubitImportance(circuit)
    var state = MappingState(
      logicalToPhysical = initialPlacement,
      physicalToLogical = initialPlacement.map(_.swap)
    )

    circuit.remainingGates.foreach {
      case g if isSingleQubitGate(g) || isMeasurementLike(g) =>
        emitted ++= lowerAndMapSingleOrMeasure(g, q => state.physicalOf(q), device, config.strictGateSetCheck)

      case g if isTwoQubitGate(g) =>
        val Vector(l0, l1) = gateQubits(g)
        val p0 = state.physicalOf(l0)
        val p1 = state.physicalOf(l1)

        if (!region.hasEdge(p0, p1)) {
          val path = region.shortestPath(p0, p1, routingEdgeCost(device, calibration, config))
            .getOrElse(throw new IllegalArgumentException(s"No routing path inside HAQA region between physical qubits $p0 and $p1"))

          if (path.length >= 3) {
            val leftMoves = path.sliding(2).toVector.dropRight(1).map { case Vector(a, b) => (a, b) }
            val rightMoves = path.reverse.sliding(2).toVector.dropRight(1).map { case Vector(a, b) => (a, b) }

            val moveLogicalLeft = logicalImportance.getOrElse(l0, 0.0)
            val moveLogicalRight = logicalImportance.getOrElse(l1, 0.0)

            val leftCost = leftMoves.map { case (a, b) => swapMoveCost(device, calibration, a, b, config) }.sum +
              config.moveImportancePenaltyWeight * moveLogicalLeft
            val rightCost = rightMoves.map { case (a, b) => swapMoveCost(device, calibration, a, b, config) }.sum +
              config.moveImportancePenaltyWeight * moveLogicalRight

            val chosenMoves = if (leftCost <= rightCost) leftMoves else rightMoves
            chosenMoves.foreach {
              case (a, b) =>
                emitted ++= lowerSwap(a, b, device, config.strictGateSetCheck)
                state = state.swap(a, b)
            }
          }
        }

        val pp0 = state.physicalOf(l0)
        val pp1 = state.physicalOf(l1)
        if (!region.hasEdge(pp0, pp1)) {
          throw new IllegalStateException(s"Routing failed for gate $g: endpoints $pp0 and $pp1 are still non-adjacent")
        }
        emitted ++= lowerAndMapTwoQubit(g, pp0, pp1, device, config.strictGateSetCheck)

      case g if isThreeQubitGate(g) =>
        emitted ++= lowerAndMapThreeQubit(g, q => state.physicalOf(q), device, config.strictGateSetCheck)

      case other =>
        emitted += remapGate(other, q => state.physicalOf(q))
    }

    Circuit(emitted.toList, region.maxPhysicalIndex + 1, circuit.name)
  }

  private def routingEdgeCost(
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      config: Config
  ): PhysicalEdge => Double = { edge =>
    swapMoveCost(device, calibration, edge.u, edge.v, config)
  }

  private def swapMoveCost(
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      a: Int,
      b: Int,
      config: Config
  ): Double = {
    swapSynthesisCost(PhysicalEdge(a, b), device, calibration, config) + config.swapLengthWeight
  }

  private val TwoQubitGateKinds: Set[String] = Set("cx", "cy", "cz", "ch", "swap", "cp", "crx", "cry", "crz", "cu", "rzz")

  private def normalizeInputGate(g: Gate): List[Gate] = g match {
    case U2(phi, lambda, q) => List(U("pi/2", phi, lambda, q))
    case U3(theta, phi, lambda, q) => List(U(theta, phi, lambda, q))
    case other => List(other)
  }

  private def lowerAndMapSingleOrMeasure(
      g: Gate,
      mapQ: Int => Int,
      device: QuantumDevice,
      strict: Boolean
  ): List[Gate] = {
    val pg = remapGate(g, mapQ)
    pg match {
      case Phase(theta, q) if !device.supportedGateKinds.contains("phase") && device.supportedGateKinds.contains("rz") =>
        List(RZ(theta, q)) 
      case gate =>
        val kind = gateKind(gate)
        if (!strict || device.supportedGateKinds.contains(kind) || kind == "measure" || kind == "reset") List(gate)
        else throw new IllegalArgumentException(s"Device gate set does not support single-qubit gate '$kind' and no lowering is implemented")
    }
  }

  private def lowerAndMapThreeQubit(
      g: Gate,
      mapQ: Int => Int,
      device: QuantumDevice,
      strict: Boolean
  ): List[Gate] = {
    val mapped = remapGate(g, mapQ)
    val kind = gateKind(mapped)
    if (!strict || device.supportedGateKinds.contains(kind)) List(mapped)
    else throw new IllegalArgumentException(s"Device gate set does not support three-qubit gate '$kind' and no lowering is implemented")
  }

  private def lowerAndMapTwoQubit(
      g: Gate,
      p0: Int,
      p1: Int,
      device: QuantumDevice,
      strict: Boolean
  ): List[Gate] = g match {
    case CX(_, _) if device.supportedGateKinds.contains("cx") => List(CX(p0, p1))
    case CX(_, _) if device.supportedGateKinds.contains("cz") => List(H(p1), CZ(p0, p1), H(p1))

    case CZ(_, _) if device.supportedGateKinds.contains("cz") => List(CZ(p0, p1))
    case CZ(_, _) if device.supportedGateKinds.contains("cx") => List(H(p1), CX(p0, p1), H(p1))

    case CY(_, _) if device.supportedGateKinds.contains("cy") => List(CY(p0, p1))
    case CY(_, _) if device.supportedGateKinds.contains("cx") => List(SDG(p1), CX(p0, p1), S(p1))
    case CY(_, _) if device.supportedGateKinds.contains("cz") => List(SDG(p1), H(p1), CZ(p0, p1), H(p1), S(p1))

    case Swap(_, _) => lowerSwap(p0, p1, device, strict)

    case CP(_, theta, _) if device.supportedGateKinds.contains("cp") => List(CP(p0, theta, p1))
    case CRX(_, theta, _) if device.supportedGateKinds.contains("crx") => List(CRX(p0, theta, p1))
    case CRY(_, theta, _) if device.supportedGateKinds.contains("cry") => List(CRY(p0, theta, p1))
    case CRZ(_, theta, _) if device.supportedGateKinds.contains("crz") => List(CRZ(p0, theta, p1))
    case CH(_, _) if device.supportedGateKinds.contains("ch") => List(CH(p0, p1))
    case CU(_, theta, phi, lambda, _) if device.supportedGateKinds.contains("cu") => List(CU(p0, theta, phi, lambda, p1))
    case NamedGate(name, params, _) => List(NamedGate(name, params, Vector(p0, p1)))

    case other =>
      val qs = gateQubits(other)
      val mapped = remapGate(other, { q => if (q == qs.head) p0 else p1 })
      val kind = gateKind(mapped)
      if (!strict) List(mapped)
      else throw new IllegalArgumentException(s"Device gate set does not support two-qubit gate '$kind' and no lowering is implemented")
  }

  private def lowerSwap(a: Int, b: Int, device: QuantumDevice, strict: Boolean): List[Gate] = {
    if (device.supportedGateKinds.contains("swap")) List(Swap(a, b))
    else if (device.supportedGateKinds.contains("cx")) {
      List(CX(a, b), CX(b, a), CX(a, b))
    } else if (device.supportedGateKinds.contains("cz")) {
      List(
        H(b), CZ(a, b), H(b),
        H(a), CZ(a, b), H(a),
        H(b), CZ(a, b), H(b)
      )
    } else if (!strict) {
      List(Swap(a, b))
    } else {
      throw new IllegalArgumentException("Device gate set does not support swap/cx/cz, so HAQA routing cannot materialize SWAP operations")
    }
  }

  private def buildInteractionGraph(circuit: Circuit): Map[PhysicalEdge, Double] = {
    val counts = mutable.Map.empty[PhysicalEdge, Double].withDefaultValue(0.0)
    circuit.remainingGates.foreach {
      case CX(a, b) => counts(logicalEdge(a, b)) += 1.0
      case CY(a, b) => counts(logicalEdge(a, b)) += 1.0
      case CZ(a, b) => counts(logicalEdge(a, b)) += 1.0
      case CH(a, b) => counts(logicalEdge(a, b)) += 1.0
      case Swap(a, b) => counts(logicalEdge(a, b)) += 1.0
      case CP(a, _, b) => counts(logicalEdge(a, b)) += 1.0
      case CRX(a, _, b) => counts(logicalEdge(a, b)) += 1.0
      case CRY(a, _, b) => counts(logicalEdge(a, b)) += 1.0
      case CRZ(a, _, b) => counts(logicalEdge(a, b)) += 1.0
      case CU(a, _, _, _, b) => counts(logicalEdge(a, b)) += 1.0
      case _ => ()
    }
    counts.toMap
  }

  private def logicalQubitImportance(circuit: Circuit): Map[Int, Double] = {
    val scores = mutable.Map.empty[Int, Double].withDefaultValue(0.0)
    circuit.remainingGates.foreach {
      case g if isTwoQubitGate(g) =>
        val qs = gateQubits(g)
        scores(qs(0)) += 1.0
        scores(qs(1)) += 1.0
      case Measure(q) => scores(q) += 0.25
      case _ => ()
    }
    scores.toMap.withDefaultValue(0.0)
  }

  private def physicalQubitScores(
      region: DeviceTopology,
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      config: Config
  ): Map[Int, Double] = {
    val tVals = region.qubits.flatMap(q => Seq(calibration.qubit(q).t1Seconds, calibration.qubit(q).t2Seconds).flatten)
    val maxCoherence = if (tVals.isEmpty) 1.0 else tVals.max

    region.qubits.map { q =>
      val edgeFids = region.neighbors.getOrElse(q, Vector.empty).flatMap { n =>
        region.edge(q, n).map(e => calibration.preferredTwoQubitGate(e, device.supportedTwoQubitGateKinds)._2.fidelity)
      }
      val avgEdgeFid = if (edgeFids.nonEmpty) edgeFids.sum / edgeFids.size else 0.5
      val coherence = {
        val qc = calibration.qubit(q)
        val cands = Seq(qc.t1Seconds, qc.t2Seconds).flatten
        if (cands.isEmpty) 0.5 else clamp01(cands.sum / cands.size / maxCoherence)
      }
      q -> (config.placementEdgeWeight * avgEdgeFid + config.placementCoherenceWeight * coherence)
    }.toMap
  }

  private def modularity(topology: DeviceTopology, partition: Vector[Set[Int]]): Double = {
    val m = topology.edges.size.toDouble
    if (m <= 0.0) 0.0
    else {
      partition.map { community =>
        val internal = topology.edges.count(e => community.contains(e.u) && community.contains(e.v)).toDouble
        val degreeSum = community.toVector.map(q => topology.degree.getOrElse(q, 0)).sum.toDouble
        internal / m - math.pow(degreeSum / (2.0 * m), 2.0)
      }.sum
    }
  }

  private def crossingEdges(topology: DeviceTopology, a: Set[Int], b: Set[Int]): Vector[PhysicalEdge] =
    topology.edges.filter { e => (a.contains(e.u) && b.contains(e.v)) || (a.contains(e.v) && b.contains(e.u)) }

  private def averageFidelity(
      edges: Iterable[PhysicalEdge],
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration
  ): Double = {
    val xs = edges.iterator.map(e => calibration.edge(e).bestFidelity(device.supportedTwoQubitGateKinds)).toVector
    if (xs.isEmpty) 0.0 else xs.sum / xs.size
  }

  private def shortestDistance(topology: DeviceTopology, a: Int, b: Int): Option[Int] = {
    topology.shortestPath(a, b, _ => 1.0).map(p => math.max(0, p.size - 1))
  }

  private def measuredLogicalQubits(circuit: Circuit): Set[Int] =
    circuit.remainingGates.collect { case Measure(q) => q }.toSet

  private def safeNegLogFidelity(fidelity: Double): Double =
    -math.log(math.max(clamp01(fidelity), 1e-12))

  private def coherencePenaltySeconds(durationSeconds: Double, qubits: Iterable[Int], calibration: GeneralDeviceCalibration): Double = {
    val reciprocals = qubits.iterator.flatMap { q =>
      val qc = calibration.qubit(q)
      Seq(qc.t1Seconds, qc.t2Seconds).flatten.map(t => 1.0 / math.max(t, 1e-12))
    }.toVector
    if (reciprocals.isEmpty || durationSeconds <= 0.0) 0.0 else durationSeconds * reciprocals.sum / reciprocals.size
  }

  private def singleGateExecutionCost(
      q: Int,
      gateKind: String,
      calibration: GeneralDeviceCalibration,
      config: Config
  ): Double = {
    val stats = calibration.singleQubitStats(q, gateKind)
    val base = config.routingSingleQErrorWeight * safeNegLogFidelity(stats.fidelity)
    val coh = config.routingCoherenceWeight * coherencePenaltySeconds(stats.durationNs.toDouble / 1e9, List(q), calibration)
    base + coh
  }

  private def twoQGateExecutionCost(
      edge: PhysicalEdge,
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      config: Config
  ): Double = {
    val (_, stats) = calibration.preferredTwoQubitGate(edge, device.supportedTwoQubitGateKinds)
    val base = config.routingTwoQErrorWeight * safeNegLogFidelity(stats.fidelity)
    val coh = config.routingCoherenceWeight * coherencePenaltySeconds(stats.durationNs.toDouble / 1e9, List(edge.u, edge.v), calibration)
    base + coh
  }

  private def swapSynthesisCost(
      edge: PhysicalEdge,
      device: QuantumDevice,
      calibration: GeneralDeviceCalibration,
      config: Config
  ): Double = {
    if (device.supportedGateKinds.contains("swap")) {
      val ec = calibration.edge(edge)
      val swapStats = ec.gateStats.get("swap").orElse(calibration.defaultTwoQubitGateStats.get("swap"))
      swapStats match {
        case Some(gs) =>
          config.routingTwoQErrorWeight * safeNegLogFidelity(gs.fidelity) +
            config.routingCoherenceWeight * coherencePenaltySeconds(gs.durationNs.toDouble / 1e9, List(edge.u, edge.v), calibration)
        case None => 3.0 * twoQGateExecutionCost(edge, device, calibration, config)
      }
    } else if (device.supportedGateKinds.contains("cx")) {
      3.0 * twoQGateExecutionCost(edge, device, calibration, config)
    } else if (device.supportedGateKinds.contains("cz")) {
      3.0 * twoQGateExecutionCost(edge, device, calibration, config) +
        2.0 * (singleGateExecutionCost(edge.u, "h", calibration, config) + singleGateExecutionCost(edge.v, "h", calibration, config))
    } else {
      3.0 * twoQGateExecutionCost(edge, device, calibration, config)
    }
  }

  private def singleGateAliases(gateKind: String): Vector[String] = gateKind.toLowerCase match {
    case "phase" => Vector("phase", "p", "rz")
    case "p" => Vector("p", "phase", "rz")
    case "h" => Vector("h", "sx", "x", "rx")
    case "s" => Vector("s", "rz")
    case "sdg" => Vector("sdg", "rz")
    case "t" => Vector("t", "rz")
    case "tdg" => Vector("tdg", "rz")
    case other => Vector(other)
  }

  private def logicalEdge(a: Int, b: Int): PhysicalEdge = PhysicalEdge(a, b)

  private def isSingleQubitGate(g: Gate): Boolean = g match {
    case X(_) | Y(_) | Z(_) | H(_) | S(_) | SDG(_) | T(_) | TDG(_) | SX(_) | SXDG(_) | Id(_) |
         Phase(_, _) | RX(_, _) | RY(_, _) | RZ(_, _) | U(_, _, _, _) => true
    case _ => false
  }

  private def isMeasurementLike(g: Gate): Boolean = g match {
    case Measure(_) | Reset(_) => true
    case _ => false
  }

  private def isTwoQubitGate(g: Gate): Boolean = g match {
    case CX(_, _) | CY(_, _) | CZ(_, _) | CH(_, _) | Swap(_, _) |
         CP(_, _, _) | CRX(_, _, _) | CRY(_, _, _) | CRZ(_, _, _) | CU(_, _, _, _, _) => true
    case _ => false
  }

  private def isThreeQubitGate(g: Gate): Boolean = g match {
    case CCX(_, _, _) => true
    case _ => false
  }

  private def gateQubits(g: Gate): Vector[Int] = g match {
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
    case CX(c, t) => Vector(c, t)
    case CY(c, t) => Vector(c, t)
    case CZ(c, t) => Vector(c, t)
    case CH(c, t) => Vector(c, t)
    case Swap(a, b) => Vector(a, b)
    case CP(c, _, t) => Vector(c, t)
    case CRX(c, _, t) => Vector(c, t)
    case CRY(c, _, t) => Vector(c, t)
    case CRZ(c, _, t) => Vector(c, t)
    case CU(c, _, _, _, t) => Vector(c, t)
    case CCX(c1, c2, t) => Vector(c1, c2, t)
    case Measure(q) => Vector(q)
    case Reset(q) => Vector(q)
    case GPhase(_) => Vector.empty
    case NamedGate(_, _, qubits) => qubits
  }

  private def remapGate(g: Gate, mapQ: Int => Int): Gate = g match {
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
    case CX(c, t) => CX(mapQ(c), mapQ(t))
    case CY(c, t) => CY(mapQ(c), mapQ(t))
    case CZ(c, t) => CZ(mapQ(c), mapQ(t))
    case CH(c, t) => CH(mapQ(c), mapQ(t))
    case Swap(a, b) => Swap(mapQ(a), mapQ(b))
    case CP(c, theta, t) => CP(mapQ(c), theta, mapQ(t))
    case CRX(c, theta, t) => CRX(mapQ(c), theta, mapQ(t))
    case CRY(c, theta, t) => CRY(mapQ(c), theta, mapQ(t))
    case CRZ(c, theta, t) => CRZ(mapQ(c), theta, mapQ(t))
    case CU(c, theta, phi, lambda, t) => CU(mapQ(c), theta, phi, lambda, mapQ(t))
    case CCX(c1, c2, t) => CCX(mapQ(c1), mapQ(c2), mapQ(t))
    case Measure(q) => Measure(mapQ(q))
    case Reset(q) => Reset(mapQ(q))
    case GPhase(theta) => GPhase(theta)
    case NamedGate(name, params, qubits) => NamedGate(name, params, qubits.map(mapQ))
  }

  private def gateKind(g: Gate): String = g match {
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

  private def clamp01(x: Double): Double = math.max(0.0, math.min(1.0, x))
}
