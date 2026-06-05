package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.device.Device
import qurator.testbed.HaqaMapper.DeviceTopology

import scala.collection.mutable

object CircuitMerger {

  final case class Config(
      seedIsolationWeight: Double = 2.0,
      interactionWeight: Double = 2.0,
      compactnessWeight: Double = 0.20,
      interProcessIsolationWeight: Double = 0.75,
      pathConflictWeight: Double = 0.50,
      localSearchRadius: Int = 2,
      maxLocalSearchPasses: Int = 24,
      interleaveRoundRobinByLayer: Boolean = true
  )

  sealed trait MergeError extends Product with Serializable {
    def message: String
  }

  object MergeError {
    final case object EmptyTopology extends MergeError {
      val message: String = "Device topology is empty"
    }

    final case class MissingCalibrationTopology(deviceType: String) extends MergeError {
      val message: String = s"Calibration does not expose a topology for $deviceType"
    }

    final case class NotEnoughPhysicalQubits(required: Int, available: Int) extends MergeError {
      val message: String =
        s"Need $required physical qubits to place the merged circuits, but the topology only exposes $available"
    }

    final case class UnroutablePlacement(circuitIndex: Int, logicalA: Int, logicalB: Int) extends MergeError {
      val message: String =
        s"Circuit $circuitIndex could not be placed on a connected region for logical qubits $logicalA and $logicalB"
    }
  }

  sealed trait CountBitOrder extends Product with Serializable
  object CountBitOrder {
    case object LeftToRight extends CountBitOrder
    case object RightToLeft extends CountBitOrder
  }

  final case class CircuitPartition(
      circuitIndex: Int,
      logicalOffset: Int,
      logicalQubitCount: Int,
      originalToMergedLogical: Vector[Int],
      originalToPhysical: Vector[Int],
      measurementBitIndices: Vector[Int],
      measuredOriginalQubits: Vector[Int],
      measuredMergedLogicalQubits: Vector[Int],
      measuredPhysicalQubits: Vector[Int]
  ) {
    lazy val mergedLogicalToPhysical: Map[Int, Int] =
      originalToMergedLogical.zip(originalToPhysical).toMap
  }

  final case class MergePlan(
      logicalCircuit: Circuit,
      deviceCircuit: Circuit,
      topology: DeviceTopology,
      mergedLogicalToPhysical: Vector[Int],
      partitions: Vector[CircuitPartition]
  ) {
    lazy val totalMeasurements: Int = partitions.iterator.map(_.measurementBitIndices.size).sum

    def bindIds[A](ids: Vector[A]): Either[String, Map[A, CircuitPartition]] =
      if (ids.size != partitions.size)
        Left(s"Expected ${partitions.size} ids for merged circuits but got ${ids.size}")
      else Right(ids.zip(partitions.sortBy(_.circuitIndex)).toMap)

    def splitCounts(
        mergedCounts: Map[String, Long],
        bitOrder: CountBitOrder = CountBitOrder.LeftToRight
    ): Either[String, Vector[Map[String, Long]]] = {
      val sortedPartitions = partitions.sortBy(_.circuitIndex)
      val buckets = Vector.fill(sortedPartitions.size)(mutable.LinkedHashMap.empty[String, Long].withDefaultValue(0L))

      mergedCounts.toList.foreach { case (rawKey, count) =>
        val key = rawKey.filterNot(_.isWhitespace)

        if (key.length != totalMeasurements) {
          return Left(
            s"Measurement key '$rawKey' has width ${key.length}, but the merge plan expects $totalMeasurements measurement bits"
          )
        }

        sortedPartitions.foreach { partition =>
          val projected = partition.measurementBitIndices.iterator.map { bitIndex =>
            val idx = bitOrder match {
              case CountBitOrder.LeftToRight => bitIndex
              case CountBitOrder.RightToLeft => key.length - 1 - bitIndex
            }
            key.charAt(idx)
          }.mkString

          buckets(partition.circuitIndex).update(projected, buckets(partition.circuitIndex)(projected) + count)
        }
      }

      Right(buckets.map(_.toMap))
    }
  }

  final case class MergeTarget(
      device: Device,
      topology: DeviceTopology
  )

  def merge(
      circuits: Vector[Circuit],
      topology: DeviceTopology,
      config: Config = Config()
  ): Either[MergeError, MergePlan] = {
    if (topology.qubits.isEmpty) return Left(MergeError.EmptyTopology)

    val requiredQubits = circuits.iterator.map(_.qubits).sum
    if (requiredQubits > topology.qubits.size)
      return Left(MergeError.NotEnoughPhysicalQubits(requiredQubits, topology.qubits.size))

    val analyses = circuits.zipWithIndex.map { case (circuit, idx) => analyzeCircuit(idx, circuit) }
    val metrics = TopologyMetrics(topology)
    val placements = placeCircuits(analyses, topology, metrics, config)

    validatePlacements(analyses, placements, metrics).left.map {
      case (circuitIndex, a, b) => MergeError.UnroutablePlacement(circuitIndex, a, b)
    }.map { _ =>
      buildPlan(circuits, topology, placements, config)
    }
  }

  def merge(
      circuits: Vector[Circuit],
      target: MergeTarget,
      config: Config
  ): Either[MergeError, MergePlan] =
    merge(circuits, target.topology, config)

  def merge(
      circuits: Vector[Circuit],
      target: MergeTarget
  ): Either[MergeError, MergePlan] =
    merge(circuits, target, Config())

  def merge(
      circuits: Vector[Circuit],
      calibration: DeviceCalibration,
      config: Config
  ): Either[MergeError, MergePlan] =
    topologyOf(calibration)
      .toRight(MergeError.MissingCalibrationTopology(calibration.getClass.getSimpleName))
      .flatMap { topo =>
        merge(circuits, DeviceTopology.fromEdges(topo.normalizedEdges, topo.qubits), config)
      }

  def merge(
      circuits: Vector[Circuit],
      calibration: DeviceCalibration
  ): Either[MergeError, MergePlan] =
    merge(circuits, calibration, Config())

  def merge(
      circuits: Vector[Circuit],
      device: Device,
      calibration: DeviceCalibration,
      config: Config
  ): Either[MergeError, MergePlan] =
    topologyOf(calibration)
      .toRight(MergeError.MissingCalibrationTopology(calibration.getClass.getSimpleName))
      .flatMap { topo =>
        merge(circuits, MergeTarget(device, DeviceTopology.fromEdges(topo.normalizedEdges, topo.qubits)), config)
      }

  def merge(
      circuits: Vector[Circuit],
      device: Device,
      calibration: DeviceCalibration
  ): Either[MergeError, MergePlan] =
    merge(circuits, device, calibration, Config())

  private final case class CircuitAnalysis(
      circuitIndex: Int,
      qubits: Int,
      interactionWeights: Map[(Int, Int), Int],
      importance: Vector[Double]
  ) {
    val totalInteractionWeight: Int = interactionWeights.values.sum

    def weight(a: Int, b: Int): Int =
      interactionWeights.getOrElse(normalizePair(a, b), 0)
  }

  private final case class TopologyMetrics(
      distance: Map[(Int, Int), Int],
      shortestPaths: Map[(Int, Int), Vector[Int]]
  ) {
    def dist(a: Int, b: Int): Option[Int] =
      if (a == b) Some(0) else distance.get(normalizePair(a, b))

    def shortestPath(a: Int, b: Int): Option[Vector[Int]] =
      if (a == b) Some(Vector(a)) else shortestPaths.get(normalizePair(a, b))
  }

  private object TopologyMetrics {
    def apply(topology: DeviceTopology): TopologyMetrics = {
      val distance = mutable.Map.empty[(Int, Int), Int]
      val paths = mutable.Map.empty[(Int, Int), Vector[Int]]

      topology.qubits.foreach { start =>
        val prev = mutable.Map.empty[Int, Int]
        val dist = mutable.Map(start -> 0)
        val queue = mutable.Queue(start)

        while (queue.nonEmpty) {
          val current = queue.dequeue()
          topology.neighbors.getOrElse(current, Vector.empty).foreach { next =>
            if (!dist.contains(next)) {
              dist(next) = dist(current) + 1
              prev(next) = current
              queue.enqueue(next)
            }
          }
        }

        topology.qubits.foreach { goal =>
          if (goal != start && dist.contains(goal)) {
            val key = normalizePair(start, goal)
            if (!distance.contains(key)) {
              distance(key) = dist(goal)
              paths(key) = reconstructPath(start, goal, prev.toMap)
            }
          }
        }
      }

      TopologyMetrics(distance.toMap, paths.toMap)
    }

    private def reconstructPath(start: Int, goal: Int, prev: Map[Int, Int]): Vector[Int] = {
      val out = mutable.ArrayBuffer(goal)
      var current = goal
      while (current != start) {
        current = prev(current)
        out += current
      }
      out.reverse.toVector
    }
  }

  private def analyzeCircuit(circuitIndex: Int, circuit: Circuit): CircuitAnalysis = {
    val interactionWeights = mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    val importance = Array.fill(circuit.qubits)(0.0)

    circuit.remainingGates.foreach {
      case gate if isTwoQubitGate(gate) =>
        val qs = gateQubits(gate)
        val key = normalizePair(qs(0), qs(1))
        interactionWeights.update(key, interactionWeights(key) + 1)
        importance(qs(0)) += 1.0
        importance(qs(1)) += 1.0

      case Measure(q) =>
        importance(q) += 0.25

      case gate if gateQubits(gate).nonEmpty =>
        gateQubits(gate).foreach(q => importance(q) += 0.05)

      case _ =>
        ()
    }

    CircuitAnalysis(
      circuitIndex = circuitIndex,
      qubits = circuit.qubits,
      interactionWeights = interactionWeights.toMap,
      importance = importance.toVector
    )
  }

  private def placeCircuits(
      analyses: Vector[CircuitAnalysis],
      topology: DeviceTopology,
      metrics: TopologyMetrics,
      config: Config
  ): Vector[Vector[Int]] = {
    val placementByCircuit = Array.fill[Vector[Int]](analyses.size)(Vector.empty)
    val usedPhysical = mutable.Set.empty[Int]
    val circuitOrder =
      analyses.sortBy(a => (-a.qubits, -a.totalInteractionWeight, a.circuitIndex))
    val placedSeeds = mutable.ArrayBuffer.empty[Int]

    circuitOrder.foreach { analysis =>
      val logicalOrder = (0 until analysis.qubits).toVector.sortBy(q => (-analysis.importance(q), q))
      val seedLogical = logicalOrder.headOption.getOrElse(0)

      val seedPhysical = chooseSeed(topology, usedPhysical.toSet, placedSeeds.toVector, metrics, config)
      val assigned = mutable.Map(seedLogical -> seedPhysical)
      usedPhysical += seedPhysical
      placedSeeds += seedPhysical

      logicalOrder.tail.foreach { logicalQubit =>
        val bestPhysical =
          topology.qubits.iterator
            .filterNot(usedPhysical.contains)
            .minBy { candidate =>
              placementScore(
                circuitIndex = analysis.circuitIndex,
                logicalQubit = logicalQubit,
                candidate = candidate,
                assignedWithinCircuit = assigned.toMap,
                placements = placementByCircuit.toVector,
                analyses = analyses,
                metrics = metrics,
                config = config
              )
            }

        assigned(logicalQubit) = bestPhysical
        usedPhysical += bestPhysical
      }

      placementByCircuit(analysis.circuitIndex) =
        Vector.tabulate(analysis.qubits)(logical => assigned(logical))
    }

    greedyLocalImprove(analyses, topology, metrics, placementByCircuit.toVector, config)
  }

  private def chooseSeed(
      topology: DeviceTopology,
      usedPhysical: Set[Int],
      existingSeeds: Vector[Int],
      metrics: TopologyMetrics,
      config: Config
  ): Int =
    topology.qubits
      .iterator
      .filterNot(usedPhysical.contains)
      .maxBy { candidate =>
        val minSeedDistance =
          if (existingSeeds.isEmpty) 0.0
          else existingSeeds.iterator.flatMap(seed => metrics.dist(candidate, seed)).minOption.map(_.toDouble).getOrElse(0.0)

        val degreeBoost = topology.degree.getOrElse(candidate, 0).toDouble * 0.10
        config.seedIsolationWeight * minSeedDistance + degreeBoost
      }

  private def placementScore(
      circuitIndex: Int,
      logicalQubit: Int,
      candidate: Int,
      assignedWithinCircuit: Map[Int, Int],
      placements: Vector[Vector[Int]],
      analyses: Vector[CircuitAnalysis],
      metrics: TopologyMetrics,
      config: Config
  ): Double = {
    val analysis = analyses(circuitIndex)

    val intraWeightedDistance =
      assignedWithinCircuit.iterator.map { case (otherLogical, phys) =>
        val weight = analysis.weight(logicalQubit, otherLogical).toDouble
        val dist = metrics.dist(candidate, phys).map(_.toDouble).getOrElse(1e6)
        weight * dist
      }.sum

    val compactness =
      if (assignedWithinCircuit.isEmpty) 0.0
      else assignedWithinCircuit.valuesIterator.flatMap(phys => metrics.dist(candidate, phys)).map(_.toDouble).sum

    val foreignPlacedQubits =
      placements.iterator.zipWithIndex
        .filter { case (phys, idx) => idx != circuitIndex && phys.nonEmpty }
        .flatMap(_._1)
        .toVector

    val isolation =
      if (foreignPlacedQubits.isEmpty) 0.0
      else foreignPlacedQubits.iterator.flatMap(phys => metrics.dist(candidate, phys)).map(_.toDouble).sum / foreignPlacedQubits.size.toDouble

    config.interactionWeight * intraWeightedDistance +
      config.compactnessWeight * compactness -
      config.interProcessIsolationWeight * isolation
  }

  private def greedyLocalImprove(
      analyses: Vector[CircuitAnalysis],
      topology: DeviceTopology,
      metrics: TopologyMetrics,
      initial: Vector[Vector[Int]],
      config: Config
  ): Vector[Vector[Int]] = {
    var current = initial
    var currentCost = totalPlacementCost(analyses, current, metrics, config)
    var pass = 0

    while (pass < config.maxLocalSearchPasses) {
      val used = usedPhysical(current)
      val free = topology.qubits.filterNot(used.contains)
      var best: Option[(Double, Vector[Vector[Int]])] = None

      current.indices.foreach { circuitIndex =>
        current(circuitIndex).indices.foreach { logicalQubit =>
          val currentPhys = current(circuitIndex)(logicalQubit)

          free.iterator
            .filter(candidate => metrics.dist(currentPhys, candidate).exists(_ <= config.localSearchRadius))
            .foreach { candidate =>
              val updatedCircuitPlacement = current(circuitIndex).updated(logicalQubit, candidate)
              val trial = current.updated(circuitIndex, updatedCircuitPlacement)
              val cost = totalPlacementCost(analyses, trial, metrics, config)

              if (cost + 1e-9 < currentCost && best.forall(_._1 > cost)) {
                best = Some(cost -> trial)
              }
            }
        }
      }

      best match {
        case Some((cost, improved)) =>
          current = improved
          currentCost = cost
          pass += 1
        case None =>
          pass = config.maxLocalSearchPasses
      }
    }

    current
  }

  private def totalPlacementCost(
      analyses: Vector[CircuitAnalysis],
      placements: Vector[Vector[Int]],
      metrics: TopologyMetrics,
      config: Config
  ): Double = {
    val occupiedByPhysical = mutable.Map.empty[Int, Int]
    placements.iterator.zipWithIndex.foreach { case (physicals, circuitIndex) =>
      physicals.foreach(phys => occupiedByPhysical.update(phys, circuitIndex))
    }

    val intraCost =
      analyses.iterator.map { analysis =>
        val placement = placements(analysis.circuitIndex)
        analysis.interactionWeights.iterator.map { case ((a, b), weight) =>
          metrics.dist(placement(a), placement(b)).map(_.toDouble * weight.toDouble).getOrElse(1e6)
        }.sum
      }.sum

    val compactnessCost =
      analyses.iterator.map { analysis =>
        val placement = placements(analysis.circuitIndex)
        if (placement.size <= 1) 0.0
        else {
          val pairs = for {
            i <- placement.indices
            j <- (i + 1) until placement.size
          } yield metrics.dist(placement(i), placement(j)).map(_.toDouble).getOrElse(1e6)
          if (pairs.isEmpty) 0.0 else pairs.sum / pairs.size.toDouble
        }
      }.sum

    val isolationReward = {
      val circuitPairs = for {
        a <- placements.indices
        b <- (a + 1) until placements.size
      } yield {
        val lhs = placements(a)
        val rhs = placements(b)
        val distances = for {
          qa <- lhs.iterator
          qb <- rhs.iterator
          d <- metrics.dist(qa, qb).iterator
        } yield d.toDouble

        if (distances.isEmpty) 0.0 else distances.sum / distances.size.toDouble
      }
      if (circuitPairs.isEmpty) 0.0 else circuitPairs.sum
    }

    val pathConflictCost =
      analyses.iterator.map { analysis =>
        val placement = placements(analysis.circuitIndex)
        analysis.interactionWeights.iterator.map { case ((a, b), weight) =>
          metrics.shortestPath(placement(a), placement(b)).toVector.flatMap(_.drop(1).dropRight(1)).count { phys =>
            occupiedByPhysical.get(phys).exists(_ != analysis.circuitIndex)
          }.toDouble * weight.toDouble
        }.sum
      }.sum

    config.interactionWeight * intraCost +
      config.compactnessWeight * compactnessCost +
      config.pathConflictWeight * pathConflictCost -
      config.interProcessIsolationWeight * isolationReward
  }

  private def usedPhysical(placements: Vector[Vector[Int]]): Set[Int] =
    placements.iterator.flatMap(identity).toSet

  private def validatePlacements(
      analyses: Vector[CircuitAnalysis],
      placements: Vector[Vector[Int]],
      metrics: TopologyMetrics
  ): Either[(Int, Int, Int), Unit] = {
    analyses.foreach { analysis =>
      val placement = placements(analysis.circuitIndex)
      analysis.interactionWeights.keys.foreach { case (a, b) =>
        if (metrics.shortestPath(placement(a), placement(b)).isEmpty) {
          return Left((analysis.circuitIndex, a, b))
        }
      }
    }

    Right(())
  }

  private def buildPlan(
      circuits: Vector[Circuit],
      topology: DeviceTopology,
      placements: Vector[Vector[Int]],
      config: Config
  ): MergePlan = {
    val offsets = circuits.scanLeft(0)(_ + _.qubits).dropRight(1).toVector
    val logicalPartitions = placements.indices.map { circuitIndex =>
      Vector.tabulate(circuits(circuitIndex).qubits)(logical => offsets(circuitIndex) + logical)
    }.toVector

    val logicalRemappedCircuits = circuits.indices.map { circuitIndex =>
      val offset = offsets(circuitIndex)
      circuits(circuitIndex).copy(
        remainingGates = circuits(circuitIndex).remainingGates.map(remapGate(_, _ + offset)),
        qubits = circuits(circuitIndex).qubits
      )
    }.toVector

    val logicalCircuit =
      if (config.interleaveRoundRobinByLayer) roundRobinMerge(logicalRemappedCircuits, circuits.map(_.qubits).sum)
      else combineSequentially(logicalRemappedCircuits, circuits.map(_.qubits).sum)

    val mergedLogicalToPhysical =
      logicalPartitions.indices
        .flatMap { circuitIndex =>
          logicalPartitions(circuitIndex).zip(placements(circuitIndex)).sortBy(_._1).map(_._2)
        }
        .toVector

    val deviceCircuit = logicalCircuit.copy(
      remainingGates = logicalCircuit.remainingGates.map(remapGate(_, mergedLogicalToPhysical)),
      qubits = topology.maxPhysicalIndex + 1,
      name =
        if (logicalCircuit.name.nonEmpty) logicalCircuit.name
        else "merged_device_circuit"
    )

    val measurementBitsByCircuit = Array.fill(circuits.size)(mutable.ArrayBuffer.empty[Int])
    val measuredOriginalByCircuit = Array.fill(circuits.size)(mutable.ArrayBuffer.empty[Int])
    val measuredMergedByCircuit = Array.fill(circuits.size)(mutable.ArrayBuffer.empty[Int])
    val measuredPhysicalByCircuit = Array.fill(circuits.size)(mutable.ArrayBuffer.empty[Int])

    val mergedLogicalLookup =
      circuits.indices.flatMap { circuitIndex =>
        (0 until circuits(circuitIndex).qubits).map { originalLogical =>
          val mergedLogical = offsets(circuitIndex) + originalLogical
          mergedLogical -> (circuitIndex -> originalLogical)
        }
      }.toMap

    var measurementIndex = 0
    logicalCircuit.remainingGates.foreach {
      case Measure(mergedLogical) =>
        val (circuitIndex, originalLogical) = mergedLogicalLookup(mergedLogical)
        measurementBitsByCircuit(circuitIndex) += measurementIndex
        measuredOriginalByCircuit(circuitIndex) += originalLogical
        measuredMergedByCircuit(circuitIndex) += mergedLogical
        measuredPhysicalByCircuit(circuitIndex) += placements(circuitIndex)(originalLogical)
        measurementIndex += 1

      case _ =>
        ()
    }

    val partitions =
      circuits.indices.map { circuitIndex =>
        CircuitPartition(
          circuitIndex = circuitIndex,
          logicalOffset = offsets(circuitIndex),
          logicalQubitCount = circuits(circuitIndex).qubits,
          originalToMergedLogical = logicalPartitions(circuitIndex),
          originalToPhysical = placements(circuitIndex),
          measurementBitIndices = measurementBitsByCircuit(circuitIndex).toVector,
          measuredOriginalQubits = measuredOriginalByCircuit(circuitIndex).toVector,
          measuredMergedLogicalQubits = measuredMergedByCircuit(circuitIndex).toVector,
          measuredPhysicalQubits = measuredPhysicalByCircuit(circuitIndex).toVector
        )
      }.toVector

    MergePlan(
      logicalCircuit = logicalCircuit.copy(name = mergedName(circuits, suffix = "logical")),
      deviceCircuit = deviceCircuit.copy(name = mergedName(circuits, suffix = "device")),
      topology = topology,
      mergedLogicalToPhysical = mergedLogicalToPhysical,
      partitions = partitions
    )
  }

  private def roundRobinMerge(remappedCircuits: Vector[Circuit], totalQubits: Int): Circuit = {
    val layers = remappedCircuits.map(layerize)
    val pointers = Array.fill(remappedCircuits.size)(0)
    val mergedGates = mutable.ArrayBuffer.empty[Gate]

    while (pointers.indices.exists(i => pointers(i) < layers(i).size)) {
      pointers.indices.foreach { circuitIndex =>
        val layerIdx = pointers(circuitIndex)
        if (layerIdx < layers(circuitIndex).size) {
          mergedGates ++= layers(circuitIndex)(layerIdx)
          pointers(circuitIndex) = layerIdx + 1
        }
      }
    }

    Circuit(mergedGates.toList, totalQubits)
  }

  private def combineSequentially(remappedCircuits: Vector[Circuit], totalQubits: Int): Circuit =
    Circuit(remappedCircuits.iterator.flatMap(_.remainingGates).toList, totalQubits)

  private def layerize(circuit: Circuit): Vector[Vector[Gate]] = {
    val frontiers = mutable.Map.empty[Int, Int].withDefaultValue(0)
    val layers = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[Gate]]

    circuit.remainingGates.foreach { gate =>
      val qs = gateQubits(gate)
      val layerIndex =
        if (qs.isEmpty) {
          if (layers.isEmpty) 0 else layers.size - 1
        } else {
          qs.iterator.map(frontiers).max
        }

      while (layers.size <= layerIndex) {
        layers += mutable.ArrayBuffer.empty[Gate]
      }

      layers(layerIndex) += gate

      qs.foreach(q => frontiers.update(q, layerIndex + 1))
    }

    layers.map(_.toVector).toVector
  }

  private def mergedName(circuits: Vector[Circuit], suffix: String): String = {
    val nonEmptyNames = circuits.iterator.map(_.name).filter(_.nonEmpty).toVector
    val stem =
      if (nonEmptyNames.nonEmpty) nonEmptyNames.mkString("merge(", ",", ")")
      else s"merge_${circuits.size}_circuits"
    s"${stem}_$suffix"
  }

  private def normalizePair(a: Int, b: Int): (Int, Int) =
    if (a <= b) (a, b) else (b, a)

  private def isTwoQubitGate(g: Gate): Boolean = g match {
    case CX(_, _) | CY(_, _) | CZ(_, _) | CH(_, _) | Swap(_, _) |
         CP(_, _, _) | CRX(_, _, _) | CRY(_, _, _) | CRZ(_, _, _) | CU(_, _, _, _, _) => true
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

  private def remapGate(g: Gate, logicalToPhysical: Vector[Int]): Gate =
    remapGate(g, logical => logicalToPhysical(logical))
}
