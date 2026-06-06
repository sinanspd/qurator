package qurator.util

import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.device.Device
import qurator.testbed.HaqaMapper.DeviceTopology

import scala.collection.mutable

object HaloCircuitMerger {

  final case class Config(
      seedIsolationWeight: Double = 2.0,
      interactionWeight: Double = 2.0,
      helperAccessibilityWeight: Double = 1.25,
      compactnessWeight: Double = 0.20,
      interProcessIsolationWeight: Double = 0.75,
      pathConflictWeight: Double = 0.50,
      firstSeedCentralityWeight: Double = 1.0,
      firstSeedHelperSlackWeight: Double = 0.75,
      measurementReadoutWeight: Double = 0.30,
      measurementCrosstalkWeight: Double = 0.20,
      measurementCrosstalkDistanceRadius: Int = 2,
      localSearchRadius: Int = 2,
      maxLocalSearchPasses: Int = 24
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
        s"Need $required physical qubits to place all data qubits, but the topology only exposes $available"
    }

    final case class InvalidProcessReference(processIndex: Int, ref: VirtualQubitRef) extends MergeError {
      val message: String =
        s"Process $processIndex references out-of-range virtual qubit ${ref.render}"
    }

    final case class UnroutablePlacement(processIndex: Int, logicalA: Int, logicalB: Int) extends MergeError {
      val message: String =
        s"Process $processIndex could not be placed on a connected region for data qubits $logicalA and $logicalB"
    }

    final case class DeadlockedOnHelpers(processIndex: Int, helperRefs: Vector[Int], availableHelpers: Int) extends MergeError {
      val message: String =
        s"Process $processIndex is blocked waiting on helpers ${helperRefs.mkString("[", ",", "]")} with only $availableHelpers helper qubits free"
    }

    final case class UnsupportedInstruction(processIndex: Int, gateName: String, arity: Int) extends MergeError {
      val message: String =
        s"Process $processIndex uses unsupported virtual gate '$gateName' with arity $arity"
    }
  }

  sealed trait CountBitOrder extends Product with Serializable
  object CountBitOrder {
    case object LeftToRight extends CountBitOrder
    case object RightToLeft extends CountBitOrder
  }

  sealed trait VirtualQubitRef extends Product with Serializable {
    def index: Int
    def render: String
  }

  object VirtualQubitRef {
    final case class Data(index: Int) extends VirtualQubitRef {
      def render: String = s"q$index"
    }

    final case class Helper(index: Int) extends VirtualQubitRef {
      def render: String = s"s$index"
    }
  }

  import VirtualQubitRef._

  sealed trait ProcessInstruction extends Product with Serializable {
    def refs: Vector[VirtualQubitRef]
  }

  object ProcessInstruction {
    final case class Op(
        name: String,
        params: Vector[String] = Vector.empty,
        refs: Vector[VirtualQubitRef]
    ) extends ProcessInstruction

    final case class Measure(ref: VirtualQubitRef) extends ProcessInstruction {
      val refs: Vector[VirtualQubitRef] = Vector(ref)
    }

    final case class Reset(ref: VirtualQubitRef) extends ProcessInstruction {
      val refs: Vector[VirtualQubitRef] = Vector(ref)
    }

    final case class Release(helperIndices: Vector[Int]) extends ProcessInstruction {
      val refs: Vector[VirtualQubitRef] = helperIndices.map(Helper(_))
    }
  }

  import ProcessInstruction.Op

  final case class ProcessCircuit(
      dataQubits: Int,
      helperQubits: Int,
      instructions: Vector[ProcessInstruction],
      name: String = ""
  )

  final case class MergeTarget(
      device: Device,
      topology: DeviceTopology
  )

  final case class ProcessPartition(
      processIndex: Int,
      dataPhysicalQubits: Vector[Int],
      measurementBitIndices: Vector[Int],
      measuredRefs: Vector[VirtualQubitRef],
      measuredPhysicalQubits: Vector[Int]
  )

  final case class HelperAssignment(
      processIndex: Int,
      helperIndex: Int,
      physicalQubit: Int,
      assignedAtGateIndex: Int,
      releasedAfterGateIndex: Int
  )

  final case class MergePlan(
      deviceCircuit: Circuit,
      topology: DeviceTopology,
      partitions: Vector[ProcessPartition],
      helperAssignments: Vector[HelperAssignment]
  ) {
    lazy val totalMeasurements: Int =
      partitions.iterator.map(_.measurementBitIndices.size).sum

    def bindIds[A](ids: Vector[A]): Either[String, Map[A, ProcessPartition]] =
      if (ids.size != partitions.size)
        Left(s"Expected ${partitions.size} ids for merged processes but got ${ids.size}")
      else Right(ids.zip(partitions.sortBy(_.processIndex)).toMap)

    def splitCounts(
        mergedCounts: Map[String, Long],
        bitOrder: CountBitOrder = CountBitOrder.LeftToRight
    ): Either[String, Vector[Map[String, Long]]] = {
      val sortedPartitions = partitions.sortBy(_.processIndex)
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

          buckets(partition.processIndex).update(projected, buckets(partition.processIndex)(projected) + count)
        }
      }

      Right(buckets.map(_.toMap))
    }
  }

  def mergeProcesses(
      processes: Vector[ProcessCircuit],
      topology: DeviceTopology,
      config: Config = Config()
  ): Either[MergeError, MergePlan] =
    mergeProcesses(processes, topology, config, ReadoutModel.empty)

  private def mergeProcesses(
      processes: Vector[ProcessCircuit],
      topology: DeviceTopology,
      config: Config,
      readoutModel: ReadoutModel
  ): Either[MergeError, MergePlan] = {
    if (topology.qubits.isEmpty) return Left(MergeError.EmptyTopology)

    validateProcesses(processes).flatMap { _ =>
      val requiredDataQubits = processes.iterator.map(_.dataQubits).sum
      if (requiredDataQubits > topology.qubits.size)
        Left(MergeError.NotEnoughPhysicalQubits(requiredDataQubits, topology.qubits.size))
      else {
        val analyses = processes.zipWithIndex.map { case (process, idx) => analyzeProcess(idx, process) }
        val metrics = TopologyMetrics(topology)
        val placements = placeDataQubits(analyses, topology, metrics, config, readoutModel)

        validatePlacements(analyses, placements, metrics)
          .flatMap(_ => scheduleProcesses(processes, placements, topology, metrics, analyses))
      }
    }
  }

  def mergeProcesses(
      processes: Vector[ProcessCircuit],
      target: MergeTarget,
      config: Config
  ): Either[MergeError, MergePlan] =
    mergeProcesses(processes, target.topology, config, ReadoutModel.empty)

  def mergeProcesses(
      processes: Vector[ProcessCircuit],
      target: MergeTarget
  ): Either[MergeError, MergePlan] =
    mergeProcesses(processes, target, Config())

  def mergeProcesses(
      processes: Vector[ProcessCircuit],
      calibration: DeviceCalibration,
      config: Config
  ): Either[MergeError, MergePlan] =
    topologyOf(calibration)
      .toRight(MergeError.MissingCalibrationTopology(calibration.getClass.getSimpleName))
      .flatMap { topo =>
        val topology = DeviceTopology.fromEdges(topo.normalizedEdges, topo.qubits)
        mergeProcesses(processes, topology, config, ReadoutModel.fromCalibration(calibration, topology.qubits.toSet))
      }

  def mergeProcesses(
      processes: Vector[ProcessCircuit],
      calibration: DeviceCalibration
  ): Either[MergeError, MergePlan] =
    mergeProcesses(processes, calibration, Config())

  def mergeProcesses(
      processes: Vector[ProcessCircuit],
      device: Device,
      calibration: DeviceCalibration,
      config: Config
  ): Either[MergeError, MergePlan] =
    topologyOf(calibration)
      .toRight(MergeError.MissingCalibrationTopology(calibration.getClass.getSimpleName))
      .flatMap { topo =>
        val topology = DeviceTopology.fromEdges(topo.normalizedEdges, topo.qubits)
        mergeProcesses(
          processes,
          MergeTarget(device, topology),
          config,
          ReadoutModel.fromCalibration(calibration, topology.qubits.toSet)
        )
      }

  private def mergeProcesses(
      processes: Vector[ProcessCircuit],
      target: MergeTarget,
      config: Config,
      readoutModel: ReadoutModel
  ): Either[MergeError, MergePlan] =
    mergeProcesses(processes, target.topology, config, readoutModel)

  def mergeProcesses(
      processes: Vector[ProcessCircuit],
      device: Device,
      calibration: DeviceCalibration
  ): Either[MergeError, MergePlan] =
    mergeProcesses(processes, device, calibration, Config())

  private final case class ProcessAnalysis(
      processIndex: Int,
      dataQubits: Int,
      dataInteractionWeights: Map[(Int, Int), Int],
      helperTouchWeights: Vector[Double],
      dataHelperWeights: Map[(Int, Int), Int],
      importance: Vector[Double],
      measuredDataQubits: Set[Int]
  ) {
    def dataWeight(a: Int, b: Int): Int =
      dataInteractionWeights.getOrElse(normalizePair(a, b), 0)

    def helperWeight(dataQubit: Int): Double =
      helperTouchWeights.lift(dataQubit).getOrElse(0.0)

    def isMeasured(dataQubit: Int): Boolean =
      measuredDataQubits.contains(dataQubit)
  }

  private final case class ReadoutModel(
      fidelityByPhysicalQubit: Map[Int, Double],
      defaultFidelity: Double
  ) {
    def fidelity(physicalQubit: Int): Double =
      clamp01(fidelityByPhysicalQubit.getOrElse(physicalQubit, defaultFidelity))

    def error(physicalQubit: Int): Double =
      1.0 - fidelity(physicalQubit)
  }

  private object ReadoutModel {
    val empty: ReadoutModel = ReadoutModel(Map.empty, 1.0)

    def fromCalibration(calibration: DeviceCalibration, allowedQubits: Set[Int]): ReadoutModel = {
      val canonical = FidelityEstimator.normalizeCalibration(calibration)
      ReadoutModel(
        fidelityByPhysicalQubit =
          canonical.readoutFidelity.iterator.collect {
            case (physicalQubit, fidelity) if allowedQubits.contains(physicalQubit) =>
              physicalQubit -> clamp01(fidelity)
          }.toMap,
        defaultFidelity = clamp01(canonical.readoutFidelityAvg.getOrElse(1.0))
      )
    }
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

  private def validateProcesses(processes: Vector[ProcessCircuit]): Either[MergeError, Unit] = {
    processes.zipWithIndex.foreach { case (process, processIndex) =>
      process.instructions.foreach { instruction =>
        instruction.refs.foreach {
          case Data(idx) if idx < 0 || idx >= process.dataQubits =>
            return Left(MergeError.InvalidProcessReference(processIndex, Data(idx)))

          case Helper(idx) if idx < 0 || idx >= process.helperQubits =>
            return Left(MergeError.InvalidProcessReference(processIndex, Helper(idx)))

          case _ =>
            ()
        }
      }
    }

    Right(())
  }

  private def analyzeProcess(processIndex: Int, process: ProcessCircuit): ProcessAnalysis = {
    val dataWeights = mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    val helperWeights = Array.fill(process.dataQubits)(0.0)
    val dataHelperWeights = mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    val importance = Array.fill(process.dataQubits)(0.0)
    val measuredDataQubits = mutable.Set.empty[Int]

    process.instructions.foreach {
      case Op(_, _, refs) =>
        refs.collect { case Data(idx) => idx }.distinct.foreach { dq =>
          importance(dq) += 0.05
        }

        val pairs = refs.distinct.combinations(2).collect { case Vector(a, b) => (a, b) }.toVector
        pairs.foreach {
          case (Data(a), Data(b)) =>
            val key = normalizePair(a, b)
            dataWeights.update(key, dataWeights(key) + 1)
            importance(a) += 1.0
            importance(b) += 1.0

          case (Data(d), Helper(h)) =>
            helperWeights(d) += 1.0
            dataHelperWeights.update((d, h), dataHelperWeights((d, h)) + 1)
            importance(d) += 0.50

          case (Helper(h), Data(d)) =>
            helperWeights(d) += 1.0
            dataHelperWeights.update((d, h), dataHelperWeights((d, h)) + 1)
            importance(d) += 0.50

          case _ =>
            ()
        }

      case ProcessInstruction.Measure(Data(idx)) =>
        importance(idx) += 0.25
        measuredDataQubits += idx

      case _ =>
        ()
    }

    ProcessAnalysis(
      processIndex = processIndex,
      dataQubits = process.dataQubits,
      dataInteractionWeights = dataWeights.toMap,
      helperTouchWeights = helperWeights.toVector,
      dataHelperWeights = dataHelperWeights.toMap,
      importance = importance.toVector,
      measuredDataQubits = measuredDataQubits.toSet
    )
  }

  private def placeDataQubits(
      analyses: Vector[ProcessAnalysis],
      topology: DeviceTopology,
      metrics: TopologyMetrics,
      config: Config,
      readoutModel: ReadoutModel
  ): Vector[Vector[Int]] = {
    val placementByProcess = Array.fill[Vector[Int]](analyses.size)(Vector.empty)
    val usedPhysical = mutable.Set.empty[Int]
    val processOrder =
      analyses.sortBy(a => (-a.dataQubits, -a.dataInteractionWeights.values.sum, a.processIndex))
    val placedSeeds = mutable.ArrayBuffer.empty[Int]

    processOrder.foreach { analysis =>
      val logicalOrder = (0 until analysis.dataQubits).toVector.sortBy(q => (-analysis.importance(q), q))
      val seedLogical = logicalOrder.headOption.getOrElse(0)
      val seedPhysical =
        chooseSeed(topology, usedPhysical.toSet, placedSeeds.toVector, analysis, seedLogical, metrics, config, readoutModel)

      val assigned = mutable.Map(seedLogical -> seedPhysical)
      usedPhysical += seedPhysical
      placedSeeds += seedPhysical

      logicalOrder.tail.foreach { logicalQubit =>
        val bestPhysical =
          topology.qubits.iterator
            .filterNot(usedPhysical.contains)
            .minBy { candidate =>
              placementScore(
                processIndex = analysis.processIndex,
                logicalQubit = logicalQubit,
                candidate = candidate,
                assignedWithinProcess = assigned.toMap,
                placements = placementByProcess.toVector,
                analyses = analyses,
                topology = topology,
                metrics = metrics,
                config = config,
                readoutModel = readoutModel
              )
            }

        assigned(logicalQubit) = bestPhysical
        usedPhysical += bestPhysical
      }

      placementByProcess(analysis.processIndex) =
        Vector.tabulate(analysis.dataQubits)(logical => assigned(logical))
    }

    greedyLocalImprove(analyses, topology, metrics, placementByProcess.toVector, config, readoutModel)
  }

  private def chooseSeed(
      topology: DeviceTopology,
      usedPhysical: Set[Int],
      existingSeeds: Vector[Int],
      analysis: ProcessAnalysis,
      seedLogical: Int,
      metrics: TopologyMetrics,
      config: Config,
      readoutModel: ReadoutModel
  ): Int = {
    val available = topology.qubits.iterator.filterNot(usedPhysical.contains).toVector

    def inverseDistanceReward(origin: Int, targets: Iterable[Int]): Double = {
      val scored =
        targets.iterator
          .filterNot(_ == origin)
          .flatMap(target => metrics.dist(origin, target).map(dist => 1.0 / (1.0 + dist.toDouble)))
          .toVector
      if (scored.isEmpty) 0.0 else scored.sum / scored.size.toDouble
    }

    def helperSlackReward(candidate: Int): Double = {
      val helperDemand = math.min(analysis.helperWeight(seedLogical), 1.0)
      if (helperDemand <= 0.0) 0.0
      else helperDemand * inverseDistanceReward(candidate, available)
    }

    def measuredReadoutReward(candidate: Int): Double =
      if (analysis.isMeasured(seedLogical)) readoutModel.fidelity(candidate) else 0.0

    available.maxBy { candidate =>
      val centralityReward = inverseDistanceReward(candidate, topology.qubits)
      val degreeBoost = topology.degree.getOrElse(candidate, 0).toDouble * 0.10
      val helperReward = helperSlackReward(candidate)
      val readoutReward = measuredReadoutReward(candidate)
      val separationReward =
        existingSeeds.iterator.flatMap(seed => metrics.dist(candidate, seed)).minOption.map(_.toDouble).getOrElse(0.0)

      val baseReward =
        degreeBoost +
          config.firstSeedCentralityWeight * centralityReward +
          config.firstSeedHelperSlackWeight * helperReward +
          config.measurementReadoutWeight * readoutReward

      if (existingSeeds.isEmpty) baseReward
      else baseReward + config.seedIsolationWeight * separationReward
    }
  }

  private def placementScore(
      processIndex: Int,
      logicalQubit: Int,
      candidate: Int,
      assignedWithinProcess: Map[Int, Int],
      placements: Vector[Vector[Int]],
      analyses: Vector[ProcessAnalysis],
      topology: DeviceTopology,
      metrics: TopologyMetrics,
      config: Config,
      readoutModel: ReadoutModel
  ): Double = {
    val analysis = analyses(processIndex)
    val futureAssigned = assignedWithinProcess.updated(logicalQubit, candidate)

    val intraWeightedDistance =
      futureAssigned.iterator.collect {
        case (otherLogical, phys) if otherLogical != logicalQubit =>
          val weight = analysis.dataWeight(logicalQubit, otherLogical).toDouble
          val dist = metrics.dist(candidate, phys).map(_.toDouble).getOrElse(1e6)
          weight * dist
      }.sum

    val compactness =
      if (futureAssigned.size <= 1) 0.0
      else futureAssigned.valuesIterator.filter(_ != candidate).flatMap(phys => metrics.dist(candidate, phys)).map(_.toDouble).sum

    val foreignPlacedQubits =
      placements.iterator.zipWithIndex
        .filter { case (phys, idx) => idx != processIndex && phys.nonEmpty }
        .flatMap(_._1)
        .toVector

    val isolation =
      if (foreignPlacedQubits.isEmpty) 0.0
      else foreignPlacedQubits.iterator.flatMap(phys => metrics.dist(candidate, phys)).map(_.toDouble).sum / foreignPlacedQubits.size.toDouble

    val occupied =
      placements.iterator.zipWithIndex
        .filter(_._2 != processIndex)
        .flatMap(_._1)
        .toSet ++ futureAssigned.values

    val freeHelpers = topology.qubits.filterNot(occupied.contains).toVector
    val helperAccess =
      if (analysis.helperWeight(logicalQubit) <= 0.0) 0.0
      else if (freeHelpers.isEmpty) 1e6
      else {
        val nearest = freeHelpers.iterator.flatMap(helper => metrics.dist(candidate, helper)).minOption.getOrElse(0)
        analysis.helperWeight(logicalQubit) * nearest.toDouble
      }

    val measuredReadoutCost =
      if (analysis.isMeasured(logicalQubit)) readoutModel.error(candidate) else 0.0

    val measurementCrosstalkCost =
      if (!analysis.isMeasured(logicalQubit) || config.measurementCrosstalkWeight <= 0.0) 0.0
      else {
        val withinProcessPenalty =
          futureAssigned.iterator.collect {
            case (otherLogical, phys) if otherLogical != logicalQubit && analysis.isMeasured(otherLogical) =>
              measurementProximityPenalty(candidate, phys, metrics, readoutModel, config)
          }.sum

        val crossProcessPenalty =
          placements.iterator.zipWithIndex.collect {
            case (otherPlacement, otherProcessIndex) if otherProcessIndex != processIndex && otherPlacement.nonEmpty =>
              val otherAnalysis = analyses(otherProcessIndex)
              otherPlacement.indices.collect {
                case otherLogical if otherAnalysis.isMeasured(otherLogical) =>
                  measurementProximityPenalty(candidate, otherPlacement(otherLogical), metrics, readoutModel, config)
              }.sum
          }.sum

        withinProcessPenalty + crossProcessPenalty
      }

    config.interactionWeight * intraWeightedDistance +
      config.helperAccessibilityWeight * helperAccess +
      config.compactnessWeight * compactness -
      config.interProcessIsolationWeight * isolation +
      config.measurementReadoutWeight * measuredReadoutCost +
      config.measurementCrosstalkWeight * measurementCrosstalkCost
  }

  private def greedyLocalImprove(
      analyses: Vector[ProcessAnalysis],
      topology: DeviceTopology,
      metrics: TopologyMetrics,
      initial: Vector[Vector[Int]],
      config: Config,
      readoutModel: ReadoutModel
  ): Vector[Vector[Int]] = {
    var current = initial
    var currentCost = totalPlacementCost(analyses, current, topology, metrics, config, readoutModel)
    var pass = 0

    while (pass < config.maxLocalSearchPasses) {
      val used = usedPhysical(current)
      val free = topology.qubits.filterNot(used.contains)
      var best: Option[(Double, Vector[Vector[Int]])] = None

      current.indices.foreach { processIndex =>
        current(processIndex).indices.foreach { logicalQubit =>
          val currentPhys = current(processIndex)(logicalQubit)

          free.iterator
            .filter(candidate => metrics.dist(currentPhys, candidate).exists(_ <= config.localSearchRadius))
            .foreach { candidate =>
              val updatedProcessPlacement = current(processIndex).updated(logicalQubit, candidate)
              val trial = current.updated(processIndex, updatedProcessPlacement)
              val cost = totalPlacementCost(analyses, trial, topology, metrics, config, readoutModel)

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
      analyses: Vector[ProcessAnalysis],
      placements: Vector[Vector[Int]],
      topology: DeviceTopology,
      metrics: TopologyMetrics,
      config: Config,
      readoutModel: ReadoutModel
  ): Double = {
    val occupiedByPhysical = mutable.Map.empty[Int, Int]
    placements.iterator.zipWithIndex.foreach { case (physicals, processIndex) =>
      physicals.foreach(phys => occupiedByPhysical.update(phys, processIndex))
    }

    val freeHelpers = topology.qubits.filterNot(occupiedByPhysical.contains).toVector

    val intraCost =
      analyses.iterator.map { analysis =>
        val placement = placements(analysis.processIndex)
        analysis.dataInteractionWeights.iterator.map { case ((a, b), weight) =>
          metrics.dist(placement(a), placement(b)).map(_.toDouble * weight.toDouble).getOrElse(1e6)
        }.sum
      }.sum

    val helperCost =
      if (freeHelpers.isEmpty && analyses.exists(_.helperTouchWeights.exists(_ > 0.0))) 1e6
      else analyses.iterator.map { analysis =>
        val placement = placements(analysis.processIndex)
        placement.indices.map { dataQubit =>
          val w = analysis.helperWeight(dataQubit)
          if (w <= 0.0 || freeHelpers.isEmpty) 0.0
          else {
            val phys = placement(dataQubit)
            val nearest = freeHelpers.iterator.flatMap(helper => metrics.dist(phys, helper)).minOption.getOrElse(0)
            w * nearest.toDouble
          }
        }.sum
      }.sum

    val compactnessCost =
      analyses.iterator.map { analysis =>
        val placement = placements(analysis.processIndex)
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
      val processPairs = for {
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
      if (processPairs.isEmpty) 0.0 else processPairs.sum
    }

    val pathConflictCost =
      analyses.iterator.map { analysis =>
        val placement = placements(analysis.processIndex)
        analysis.dataInteractionWeights.iterator.map { case ((a, b), weight) =>
          metrics.shortestPath(placement(a), placement(b)).toVector.flatMap(_.drop(1).dropRight(1)).count { phys =>
            occupiedByPhysical.get(phys).exists(_ != analysis.processIndex)
          }.toDouble * weight.toDouble
        }.sum
      }.sum

    val measurementReadoutCost =
      analyses.iterator.map { analysis =>
        val placement = placements(analysis.processIndex)
        analysis.measuredDataQubits.iterator.map { logicalQubit =>
          readoutModel.error(placement(logicalQubit))
        }.sum
      }.sum

    val measurementCrosstalkCost = {
      val measuredPhysicals =
        analyses.iterator.flatMap { analysis =>
          val placement = placements(analysis.processIndex)
          analysis.measuredDataQubits.iterator.map { logicalQubit =>
            (analysis.processIndex, placement(logicalQubit))
          }
        }.toVector

      val penalties = for {
        i <- measuredPhysicals.indices
        j <- (i + 1) until measuredPhysicals.size
      } yield {
        val (_, lhsPhysical) = measuredPhysicals(i)
        val (_, rhsPhysical) = measuredPhysicals(j)
        measurementProximityPenalty(lhsPhysical, rhsPhysical, metrics, readoutModel, config)
      }

      penalties.sum
    }

    config.interactionWeight * intraCost +
      config.helperAccessibilityWeight * helperCost +
      config.compactnessWeight * compactnessCost +
      config.pathConflictWeight * pathConflictCost -
      config.interProcessIsolationWeight * isolationReward +
      config.measurementReadoutWeight * measurementReadoutCost +
      config.measurementCrosstalkWeight * measurementCrosstalkCost
  }

  private def usedPhysical(placements: Vector[Vector[Int]]): Set[Int] =
    placements.iterator.flatMap(identity).toSet

  private def validatePlacements(
      analyses: Vector[ProcessAnalysis],
      placements: Vector[Vector[Int]],
      metrics: TopologyMetrics
  ): Either[MergeError, Unit] = {
    analyses.foreach { analysis =>
      val placement = placements(analysis.processIndex)
      analysis.dataInteractionWeights.keys.foreach { case (a, b) =>
        if (metrics.shortestPath(placement(a), placement(b)).isEmpty) {
          return Left(MergeError.UnroutablePlacement(analysis.processIndex, a, b))
        }
      }
    }
    Right(())
  }

  private def scheduleProcesses(
      processes: Vector[ProcessCircuit],
      dataPlacements: Vector[Vector[Int]],
      topology: DeviceTopology,
      metrics: TopologyMetrics,
      analyses: Vector[ProcessAnalysis]
  ): Either[MergeError, MergePlan] = {
    val emitted = mutable.ArrayBuffer.empty[Gate]
    val freeHelpers = mutable.Set.empty[Int] ++ topology.qubits.filterNot(dataPlacements.iterator.flatMap(identity).toSet)
    val helperAssignmentsByProcess = Array.fill(processes.size)(mutable.Map.empty[Int, Int])
    val measurementBitsByProcess = Array.fill(processes.size)(mutable.ArrayBuffer.empty[Int])
    val measuredRefsByProcess = Array.fill(processes.size)(mutable.ArrayBuffer.empty[VirtualQubitRef])
    val measuredPhysicalsByProcess = Array.fill(processes.size)(mutable.ArrayBuffer.empty[Int])
    val helperEvents = mutable.ArrayBuffer.empty[HelperAssignment]
    val helperStarts = mutable.Map.empty[(Int, Int), Int]
    val pointers = Array.fill(processes.size)(0)
    val finished = Array.fill(processes.size)(false)

    def releaseHelper(processIndex: Int, helperIndex: Int): Unit =
      helperAssignmentsByProcess(processIndex).remove(helperIndex).foreach { physical =>
        emitted += qurator.domain.circuit.Reset(physical)
        freeHelpers += physical
        val key = (processIndex, helperIndex)
        val assignedAt = helperStarts.remove(key).getOrElse(math.max(0, emitted.size - 1))
        helperEvents += HelperAssignment(
          processIndex = processIndex,
          helperIndex = helperIndex,
          physicalQubit = physical,
          assignedAtGateIndex = assignedAt,
          releasedAfterGateIndex = emitted.size - 1
        )
      }

    def resolveData(processIndex: Int, idx: Int): Int =
      dataPlacements(processIndex)(idx)

    def scoreHelperCandidate(
        processIndex: Int,
        helperIndex: Int,
        candidate: Int,
        currentDataRefs: Vector[Int]
    ): Double = {
      val analysis = analyses(processIndex)
      val weightedPartners =
        analysis.dataHelperWeights.collect {
          case ((dataIdx, hIdx), weight) if hIdx == helperIndex =>
            metrics.dist(dataPlacements(processIndex)(dataIdx), candidate).map(_.toDouble * weight.toDouble).getOrElse(1e6)
        }.toVector

      if (weightedPartners.nonEmpty) weightedPartners.sum
      else if (currentDataRefs.nonEmpty) {
        currentDataRefs.iterator
          .flatMap(dataIdx => metrics.dist(dataPlacements(processIndex)(dataIdx), candidate))
          .map(_.toDouble)
          .sum
      } else {
        dataPlacements(processIndex).iterator
          .flatMap(phys => metrics.dist(phys, candidate))
          .map(_.toDouble)
          .sum
      }
    }

    def ensureHelpers(
        processIndex: Int,
        refs: Vector[VirtualQubitRef]
    ): Either[MergeError, Map[VirtualQubitRef, Int]] = {
      val currentAssignments = helperAssignmentsByProcess(processIndex)
      val helperRefs = refs.collect { case Helper(idx) => idx }.distinct
      val missingHelpers = helperRefs.filterNot(currentAssignments.contains)
      val dataRefs = refs.collect { case Data(idx) => idx }.distinct

      if (missingHelpers.size > freeHelpers.size) {
        Left(MergeError.DeadlockedOnHelpers(processIndex, missingHelpers, freeHelpers.size))
      } else {
        missingHelpers.foreach { helperIndex =>
          val chosen = freeHelpers.minBy(phys => scoreHelperCandidate(processIndex, helperIndex, phys, dataRefs))
          freeHelpers -= chosen
          currentAssignments.update(helperIndex, chosen)
          helperStarts.update((processIndex, helperIndex), emitted.size)
        }

        val resolved =
          refs.map {
            case Data(idx)   => Data(idx) -> resolveData(processIndex, idx)
            case Helper(idx) => Helper(idx) -> currentAssignments(idx)
          }.toMap

        Right(resolved)
      }
    }

    def finishProcess(processIndex: Int): Unit = {
      helperAssignmentsByProcess(processIndex).keys.toVector.sorted.foreach(helperIndex => releaseHelper(processIndex, helperIndex))
      finished(processIndex) = true
    }

    var measurementIndex = 0

    while (finished.contains(false)) {
      var progressed = false
      var blocked: Option[(Int, Vector[Int], Int)] = None

      (0 until processes.size).foreach { processIndex =>
        if (!finished(processIndex)) {
          val process = processes(processIndex)

          if (pointers(processIndex) >= process.instructions.size) {
            finishProcess(processIndex)
            progressed = true
          } else {
            process.instructions(pointers(processIndex)) match {
              case ProcessInstruction.Release(helperIndices) =>
                helperIndices.distinct.foreach(helperIndex => releaseHelper(processIndex, helperIndex))
                pointers(processIndex) += 1
                progressed = true

              case ProcessInstruction.Measure(ref) =>
                ensureHelpers(processIndex, Vector(ref)) match {
                  case Right(resolved) =>
                    emitted += qurator.domain.circuit.Measure(resolved(ref))
                    measurementBitsByProcess(processIndex) += measurementIndex
                    measuredRefsByProcess(processIndex) += ref
                    measuredPhysicalsByProcess(processIndex) += resolved(ref)
                    measurementIndex += 1
                    pointers(processIndex) += 1
                    progressed = true

                  case Left(MergeError.DeadlockedOnHelpers(_, helperRefs, available)) =>
                    blocked = blocked.orElse(Some((processIndex, helperRefs, available)))

                  case Left(err) =>
                    return Left(err)
                }

              case ProcessInstruction.Reset(ref) =>
                ensureHelpers(processIndex, Vector(ref)) match {
                  case Right(resolved) =>
                    emitted += qurator.domain.circuit.Reset(resolved(ref))
                    pointers(processIndex) += 1
                    progressed = true

                  case Left(MergeError.DeadlockedOnHelpers(_, helperRefs, available)) =>
                    blocked = blocked.orElse(Some((processIndex, helperRefs, available)))

                  case Left(err) =>
                    return Left(err)
                }

              case gate: Op =>
                ensureHelpers(processIndex, gate.refs) match {
                  case Right(resolved) =>
                    lowerGate(processIndex, gate, resolved) match {
                      case Right(lowered) =>
                        emitted += lowered
                        pointers(processIndex) += 1
                        progressed = true

                      case Left(err) =>
                        return Left(err)
                    }

                  case Left(MergeError.DeadlockedOnHelpers(_, helperRefs, available)) =>
                    blocked = blocked.orElse(Some((processIndex, helperRefs, available)))

                  case Left(err) =>
                    return Left(err)
                }
            }
          }
        }
      }

      if (!progressed) {
        val (processIndex, helperRefs, available) =
          blocked.getOrElse((0, Vector.empty[Int], freeHelpers.size))
        return Left(MergeError.DeadlockedOnHelpers(processIndex, helperRefs, available))
      }
    }

    val partitions =
      processes.indices.map { processIndex =>
        ProcessPartition(
          processIndex = processIndex,
          dataPhysicalQubits = dataPlacements(processIndex),
          measurementBitIndices = measurementBitsByProcess(processIndex).toVector,
          measuredRefs = measuredRefsByProcess(processIndex).toVector,
          measuredPhysicalQubits = measuredPhysicalsByProcess(processIndex).toVector
        )
      }.toVector

    Right(
      MergePlan(
        deviceCircuit = Circuit(emitted.toList, topology.maxPhysicalIndex + 1, mergedName(processes)),
        topology = topology,
        partitions = partitions,
        helperAssignments = helperEvents.toVector
      )
    )
  }

  private def lowerGate(
      processIndex: Int,
      instruction: Op,
      resolved: Map[VirtualQubitRef, Int]
  ): Either[MergeError, qurator.domain.circuit.Gate] = {
    val name = instruction.name.toLowerCase
    val qubits = instruction.refs.map(resolved)
    val params = instruction.params

    def unsupported: Left[MergeError, qurator.domain.circuit.Gate] =
      Left(MergeError.UnsupportedInstruction(processIndex, instruction.name, qubits.size))

    name match {
      case "x" if qubits.size == 1 && params.isEmpty => Right(X(qubits(0)))
      case "y" if qubits.size == 1 && params.isEmpty => Right(Y(qubits(0)))
      case "z" if qubits.size == 1 && params.isEmpty => Right(Z(qubits(0)))
      case "h" if qubits.size == 1 && params.isEmpty => Right(H(qubits(0)))
      case "s" if qubits.size == 1 && params.isEmpty => Right(S(qubits(0)))
      case "sdg" if qubits.size == 1 && params.isEmpty => Right(SDG(qubits(0)))
      case "t" if qubits.size == 1 && params.isEmpty => Right(T(qubits(0)))
      case "tdg" if qubits.size == 1 && params.isEmpty => Right(TDG(qubits(0)))
      case "sx" if qubits.size == 1 && params.isEmpty => Right(SX(qubits(0)))
      case "sxdg" if qubits.size == 1 && params.isEmpty => Right(SXDG(qubits(0)))
      case "id" if qubits.size == 1 && params.isEmpty => Right(Id(qubits(0)))
      case "p" | "phase" if qubits.size == 1 && params.size == 1 => Right(Phase(params(0), qubits(0)))
      case "rx" if qubits.size == 1 && params.size == 1 => Right(RX(params(0), qubits(0)))
      case "ry" if qubits.size == 1 && params.size == 1 => Right(RY(params(0), qubits(0)))
      case "rz" if qubits.size == 1 && params.size == 1 => Right(RZ(params(0), qubits(0)))
      case "u" if qubits.size == 1 && params.size == 3 => Right(U(params(0), params(1), params(2), qubits(0)))
      case "u2" if qubits.size == 1 && params.size == 2 => Right(U2(params(0), params(1), qubits(0)))
      case "u3" if qubits.size == 1 && params.size == 3 => Right(U3(params(0), params(1), params(2), qubits(0)))
      case "cx" | "cnot" if qubits.size == 2 && params.isEmpty => Right(CX(qubits(0), qubits(1)))
      case "cy" if qubits.size == 2 && params.isEmpty => Right(CY(qubits(0), qubits(1)))
      case "cz" if qubits.size == 2 && params.isEmpty => Right(CZ(qubits(0), qubits(1)))
      case "ch" if qubits.size == 2 && params.isEmpty => Right(CH(qubits(0), qubits(1)))
      case "swap" if qubits.size == 2 && params.isEmpty => Right(Swap(qubits(0), qubits(1)))
      case "cp" if qubits.size == 2 && params.size == 1 => Right(CP(qubits(0), params(0), qubits(1)))
      case "crx" if qubits.size == 2 && params.size == 1 => Right(CRX(qubits(0), params(0), qubits(1)))
      case "cry" if qubits.size == 2 && params.size == 1 => Right(CRY(qubits(0), params(0), qubits(1)))
      case "crz" if qubits.size == 2 && params.size == 1 => Right(CRZ(qubits(0), params(0), qubits(1)))
      case "cu" if qubits.size == 2 && params.size == 3 => Right(CU(qubits(0), params(0), params(1), params(2), qubits(1)))
      case "ccx" | "toffoli" if qubits.size == 3 && params.isEmpty => Right(CCX(qubits(0), qubits(1), qubits(2)))
      case _ if qubits.nonEmpty => Right(NamedGate(instruction.name, instruction.params, qubits))
      case _ => unsupported
    }
  }

  private def normalizePair(a: Int, b: Int): (Int, Int) =
    if (a <= b) (a, b) else (b, a)

  private def measurementProximityPenalty(
      lhsPhysical: Int,
      rhsPhysical: Int,
      metrics: TopologyMetrics,
      readoutModel: ReadoutModel,
      config: Config
  ): Double =
    if (lhsPhysical == rhsPhysical) 1e6
    else {
      metrics.dist(lhsPhysical, rhsPhysical) match {
        case Some(distance) if distance <= config.measurementCrosstalkDistanceRadius =>
          val closeness =
            (config.measurementCrosstalkDistanceRadius - distance + 1).toDouble /
              (config.measurementCrosstalkDistanceRadius + 1).toDouble
          val riskScale =
            1.0 + (readoutModel.error(lhsPhysical) + readoutModel.error(rhsPhysical)) / 2.0
          closeness * riskScale

        case _ =>
          0.0
      }
    }

  private def clamp01(value: Double): Double =
    math.max(0.0, math.min(1.0, value))

  private def mergedName(processes: Vector[ProcessCircuit]): String = {
    val nonEmptyNames = processes.iterator.map(_.name).filter(_.nonEmpty).toVector
    if (nonEmptyNames.nonEmpty) nonEmptyNames.mkString("halo(", ",", ")")
    else s"halo_merge_${processes.size}_processes"
  }
}
