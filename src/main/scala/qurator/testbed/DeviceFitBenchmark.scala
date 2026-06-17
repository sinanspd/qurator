package qurator.testbed

import cats.effect.IO
import cats.effect.syntax.all._
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import fs2.text
import org.typelevel.log4cats.Logger
import qurator.domain.ProviderClient
import qurator.domain.calibration._
import qurator.domain.circuit._
import qurator.domain.device.Device
import qurator.testbed.HaqaMapper.DeviceTopology
import qurator.util.{CircuitProcessConverter, FidelityEstimator, HaloCircuitMerger, QuantumTaskLoader, QuantumTaskLoadWarning}

import scala.collection.mutable
import scala.util.control.NonFatal

object DeviceFitBenchmark {

  final case class Settings(
      qasmFolder: Path = Path("mqt"),
      shots: Int = 1000,
      depthTolerance: Double = 0.15,
      loaderParallelism: Int = math.max(2, Runtime.getRuntime.availableProcessors()),
      deviceFetchParallelism: Int = 8,
      haloConfig: HaloCircuitMerger.Config = HaloCircuitMerger.Config()
  )

  final case class PreparedCircuit(
      name: String,
      circuit: Circuit,
      process: HaloCircuitMerger.ProcessCircuit,
      qubits: Int,
      depth: Int
  )

  final case class DepthPartition(
      circuits: Vector[PreparedCircuit]
  ) {
    val minDepth: Int = circuits.iterator.map(_.depth).min
    val maxDepth: Int = circuits.iterator.map(_.depth).max
    val totalQubits: Int = circuits.iterator.map(_.qubits).sum
    val minQubits: Int = circuits.iterator.map(_.qubits).min
  }

  final case class DeviceTarget(
      device: Device,
      calibration: DeviceCalibration,
      topology: DeviceTopology,
      canonicalCalibration: CanonicalCalibration,
      gateSet: List[Gate]
  ) {
    val label: String = s"${device.platform}/${device.platformId}"
  }

  final case class DeviceFitAttempt(
      target: DeviceTarget,
      haloPlan: HaloCircuitMerger.MergePlan,
      compiledCircuit: Circuit,
      mappedQubits: Int,
      estimatedFidelity: Double
  )

  final case class DeviceFitFailure(
      targetLabel: String,
      stage: String,
      reason: String
  ) {
    def summary: String = s"$targetLabel/$stage: $reason"
  }

  final case class IterationResult(
      circuitsMerged: Int,
      mappedQubits: Int,
      accommodatingDevices: Int,
      minEstimatedFidelity: Option[Double],
      minEstimatedFidelityDevice: Option[String],
      maxEstimatedFidelity: Option[Double],
      maxEstimatedFidelityDevice: Option[String]
  ) {
    def csvLine: String =
      List(
        circuitsMerged.toString,
        mappedQubits.toString,
        accommodatingDevices.toString,
        minEstimatedFidelity.map(formatDouble).getOrElse("NaN"),
        minEstimatedFidelityDevice.getOrElse("none"),
        maxEstimatedFidelity.map(formatDouble).getOrElse("NaN"),
        maxEstimatedFidelityDevice.getOrElse("none")
      ).map(csvField).mkString(",")
  }

  final case class Report(
      loadedCircuits: Int,
      parseWarnings: Vector[QuantumTaskLoadWarning],
      partitions: Vector[DepthPartition],
      targets: Vector[DeviceTarget],
      rows: Vector[IterationResult]
  ) {
    def csvLines: Vector[String] =
      Vector(DeviceFitBenchmark.csvHeader) ++ rows.map(_.csvLine)
  }

  val csvHeader: String =
    "circuits_merged,mapped_qubits,accommodating_devices,min_estimated_fidelity,min_device,max_estimated_fidelity,max_device"

  def writeCsv(report: Report, output: Path): IO[Unit] =
    output.parent.traverse_(Files[IO].createDirectories) *>
      Stream
        .emits(report.csvLines)
        .intersperse("\n")
        .append(Stream.emit("\n"))
        .through(text.utf8.encode)
        .through(Files[IO].writeAll(output))
        .compile
        .drain

  def run(
      clients: List[ProviderClient[IO]],
      settings: Settings = Settings()
  )(implicit logger: Logger[IO]): IO[Report] =
    for {
      loadReport <- QuantumTaskLoader.loadReport(
        QuantumTaskLoader.Settings(
          folder = settings.qasmFolder,
          shots = settings.shots,
          parallelism = settings.loaderParallelism
        )
      )
      prepared = loadReport.tasks.map(prepareTask)
      partitions = partitionBySimilarDepth(prepared, settings.depthTolerance)
      _ <- logger.info(
        s"Loaded ${prepared.size} circuits from ${settings.qasmFolder}; formed ${partitions.size} depth partitions"
      )
      targets <- fetchTargets(clients, settings.deviceFetchParallelism)
      _ <- logger.info(s"Fetched ${targets.size} devices with usable topology/calibration")
      rows <- runIterations(partitions, targets, settings)
    } yield Report(
      loadedCircuits = prepared.size,
      parseWarnings = loadReport.warnings,
      partitions = partitions,
      targets = targets,
      rows = rows
    )

  def partitionBySimilarDepth(
      circuits: Vector[PreparedCircuit],
      tolerance: Double
  ): Vector[DepthPartition] = {
    val sorted =
      circuits.sortBy(c => (c.depth, c.qubits, c.name))

    val grouped = mutable.ArrayBuffer.empty[DepthPartition]
    var current = Vector.empty[PreparedCircuit]
    var anchorDepth = 0

    sorted.foreach { circuit =>
      if (current.isEmpty) {
        current = Vector(circuit)
        anchorDepth = circuit.depth
      } else if (withinDepthTolerance(circuit.depth, anchorDepth, tolerance)) {
        current = current :+ circuit
      } else {
        grouped += DepthPartition(sortPartitionCircuits(current))
        current = Vector(circuit)
        anchorDepth = circuit.depth
      }
    }

    if (current.nonEmpty) grouped += DepthPartition(sortPartitionCircuits(current))

    grouped.toVector.sortBy(p => (p.minQubits, p.totalQubits, p.minDepth, p.maxDepth))
  }

  private def sortPartitionCircuits(circuits: Vector[PreparedCircuit]): Vector[PreparedCircuit] =
    circuits.sortBy(c => (c.qubits, c.depth, c.name))

  private def withinDepthTolerance(depth: Int, anchorDepth: Int, tolerance: Double): Boolean =
    if (anchorDepth == 0) depth == 0
    else math.abs(depth - anchorDepth).toDouble <= math.ceil(anchorDepth.toDouble * tolerance)

  private def prepareTask(task: QuantumTaskSpec): PreparedCircuit =
    PreparedCircuit(
      name = task.circuit.name,
      circuit = task.circuit,
      process = processFromCircuit(task.circuit),
      qubits = task.qubits.value,
      depth = task.depth.value
    )

  def processFromCircuit(circuit: Circuit): HaloCircuitMerger.ProcessCircuit =
    CircuitProcessConverter.liveIntervalProcessFromCircuit(circuit)

  private def fetchTargets(
      clients: List[ProviderClient[IO]],
      parallelism: Int
  )(implicit logger: Logger[IO]): IO[Vector[DeviceTarget]] =
    for {
      discovered <- clients.parTraverseN(math.max(1, parallelism)) { client =>
        client.fetchAvailableDevices.attempt.flatMap {
          case Right(devices) =>
            logger.info(s"Fetched ${devices.size} devices from ${client.provider}") *>
              IO.pure(devices.map(device => client -> device))
          case Left(error) =>
            logger.warn(error)(s"Failed to fetch devices from ${client.provider}; skipping provider") *>
              IO.pure(List.empty[(ProviderClient[IO], Device)])
        }
      }
      unique = discovered.flatten.distinctBy { case (_, device) => (device.platform, device.platformId) }
      targets <- parTraverseN(unique, parallelism) { case (client, device) =>
        fetchTarget(client, device)
      }
    } yield targets.flatten.toVector

  private def fetchTarget(
      client: ProviderClient[IO],
      device: Device
  )(implicit logger: Logger[IO]): IO[Option[DeviceTarget]] =
    client.fetchDeviceCalibration(device.platformId).attempt.flatMap {
      case Left(error) =>
        logger.warn(error)(s"Failed to fetch calibration for ${device.platform}/${device.platformId}; skipping device") *>
          IO.pure(None)

      case Right(calibration) =>
        topologyForDevice(device, calibration) match {
          case None =>
            logger.warn(s"Calibration for ${device.platform}/${device.platformId} does not expose a topology; skipping device") *>
              IO.pure(None)

          case Some(topology) if topology.qubits.isEmpty =>
            logger.warn(s"Calibration for ${device.platform}/${device.platformId} exposes an empty topology; skipping device") *>
              IO.pure(None)

          case Some(topology) =>
            IO.delay(FidelityEstimator.normalizeCalibration(calibration)).attempt.flatMap {
              case Left(error) =>
                logger.warn(error)(s"Failed to normalize calibration for ${device.platform}/${device.platformId}; skipping device") *>
                  IO.pure(None)
              case Right(canonical) =>
                IO.pure(
                  Some(
                    DeviceTarget(
                      device = device,
                      calibration = calibration,
                      topology = topology,
                      canonicalCalibration = canonical,
                      gateSet = effectiveGateSet(device, calibration, canonical)
                    )
                  )
                )
            }
        }
    }

  private def topologyForDevice(device: Device, calibration: DeviceCalibration): Option[DeviceTopology] =
    HaqaMapper.topologyFromCalibration(calibration)
      .orElse(allToAllTopologyFor(device, calibration))

  private def allToAllTopologyFor(device: Device, calibration: DeviceCalibration): Option[DeviceTopology] =
    calibration match {
      case _: IonQCalibration if device.qubits > 0 =>
        Some(completeTopology(device.qubits))
      case _: AQTCalibration if device.qubits > 0 =>
        Some(completeTopology(device.qubits))
      case _ =>
        None
    }

  private def completeTopology(qubits: Int): DeviceTopology = {
    val labels = 0 until qubits
    val edges =
      labels.flatMap { a =>
        ((a + 1) until qubits).map(b => a -> b)
      }
    DeviceTopology.fromEdges(edges, labels)
  }

  private def runIterations(
      partitions: Vector[DepthPartition],
      targets: Vector[DeviceTarget],
      settings: Settings
  )(implicit logger: Logger[IO]): IO[Vector[IterationResult]] =
    partitions.zipWithIndex.traverse { case (partition, partitionIndex) =>
      val prefixes =
        (2 to partition.circuits.size).toVector.map(k => partition.circuits.take(k))

      prefixes.traverse { prefix =>
        logger.info(
          s"Benchmarking depth partition $partitionIndex with ${prefix.size} merged circuits and ${prefix.map(_.qubits).sum} data qubits"
        ) *>
          summarizePrefix(prefix, targets, settings)
      }
    }.map(_.flatten)

  private def summarizePrefix(
      prefix: Vector[PreparedCircuit],
      targets: Vector[DeviceTarget],
      settings: Settings
  )(implicit logger: Logger[IO]): IO[IterationResult] = {
    val processes = prefix.map(_.process)
    val minimumRequiredQubits = minimumRequiredPhysicalQubits(processes)
    val candidates =
      targets.filter(_.topology.qubits.size >= minimumRequiredQubits)

    val candidateLog =
      if (candidates.isEmpty) {
        val largest = targets.map(_.topology.qubits.size).maxOption.getOrElse(0)
        logger.warn(
          s"No devices can fit ${prefix.size} merged circuits requiring at least $minimumRequiredQubits live qubits; " +
            s"largest fetched topology has $largest qubits"
        )
      } else IO.unit

    candidateLog *> candidates.traverse(target => attemptDeviceFit(processes, target, settings.haloConfig)).flatMap { attempts =>
      val failures = attempts.collect { case Left(failure) => failure }
      val successful = attempts.collect { case Right(success) => success }
      val minAttempt = successful.sortBy(_.estimatedFidelity).headOption
      val maxAttempt = successful.sortBy(_.estimatedFidelity).lastOption

      val result = IterationResult(
        circuitsMerged = prefix.size,
        mappedQubits = successful.map(_.mappedQubits).maxOption.getOrElse(minimumRequiredQubits),
        accommodatingDevices = successful.size,
        minEstimatedFidelity = minAttempt.map(_.estimatedFidelity),
        minEstimatedFidelityDevice = minAttempt.map(_.target.label),
        maxEstimatedFidelity = maxAttempt.map(_.estimatedFidelity),
        maxEstimatedFidelityDevice = maxAttempt.map(_.target.label)
      )

      val failureLog =
        if (successful.isEmpty && failures.nonEmpty) {
          logger.warn(
            s"No devices fit ${prefix.size} merged circuits requiring at least $minimumRequiredQubits live qubits. " +
              s"Top failure reasons: ${summarizeFailures(failures)}"
          )
        } else IO.unit

      failureLog.as(result)
    }
  }

  private def attemptDeviceFit(
      processes: Vector[HaloCircuitMerger.ProcessCircuit],
      target: DeviceTarget,
      haloConfig: HaloCircuitMerger.Config
  ): IO[Either[DeviceFitFailure, DeviceFitAttempt]] =
    IO.delay {
      HaloCircuitMerger.mergeProcesses(processes, target.topology, target.calibration, haloConfig) match {
        case Left(error) =>
          Left(DeviceFitFailure(target.label, "merge", error.message))

        case Right(haloPlan) =>
          compilePlacedCircuit(haloPlan.deviceCircuit, target.topology, target.gateSet) match {
            case Left(reason) =>
              Left(DeviceFitFailure(target.label, "compile", reason))

            case Right(compiled) =>
              try {
                val estimate = FidelityEstimator.score(estimatorCompatibleCircuit(compiled), target.canonicalCalibration)
                if (estimate.pTotal.isNaN || estimate.pTotal.isInfinite)
                  Left(DeviceFitFailure(target.label, "fidelity", s"non-finite pTotal=${estimate.pTotal}"))
                else
                  Right(
                    DeviceFitAttempt(
                      target = target,
                      haloPlan = haloPlan,
                      compiledCircuit = compiled,
                      mappedQubits = usedQubits(compiled).size,
                      estimatedFidelity = estimate.pTotal
                    )
                  )
              } catch {
                case NonFatal(error) =>
                  Left(DeviceFitFailure(target.label, "fidelity", error.getMessage))
              }
          }
      }
    }

  private def estimatorCompatibleCircuit(circuit: Circuit): Circuit = {
    val gates = circuit.remainingGates.flatMap(estimatorCompatibleGates)
    val nonEmptyGates =
      if (gates.nonEmpty) gates
      else usedQubits(circuit).headOption.map(q => RZ("0", q)).toList

    circuit.copy(remainingGates = nonEmptyGates)
  }

  private def estimatorCompatibleGates(gate: Gate): List[Gate] =
    gate match {
      case g @ (X(_) | H(_) | SX(_) | RX(_, _) | RY(_, _) | RZ(_, _) | CX(_, _) | CZ(_, _) | Swap(_, _) | CRZ(_, _, _) | Measure(_)) =>
        List(g)
      case Y(q) => List(RZ("pi", q), X(q))
      case Z(q) => List(RZ("pi", q))
      case S(q) => List(RZ("pi/2", q))
      case SDG(q) => List(RZ("-pi/2", q))
      case T(q) => List(RZ("pi/4", q))
      case TDG(q) => List(RZ("-pi/4", q))
      case SXDG(q) => List(SX(q))
      case Id(q) => List(RZ("0", q))
      case Phase(theta, q) => List(RZ(theta, q))
      case U(theta, phi, lambda, q) => List(RZ(lambda, q), RX(theta, q), RZ(phi, q))
      case U2(phi, lambda, q) => List(RZ(lambda, q), RX("pi/2", q), RZ(phi, q))
      case U3(theta, phi, lambda, q) => List(RZ(lambda, q), RX(theta, q), RZ(phi, q))
      case CY(c, t) => List(SDG(t), CX(c, t), S(t))
      case CH(c, t) => List(H(t), CX(c, t), H(t))
      case CP(c, theta, t) => List(CRZ(c, theta, t))
      case CRX(c, _, t) => List(CX(c, t))
      case CRY(c, _, t) => List(CX(c, t))
      case CU(c, _, _, _, t) => List(CX(c, t))
      case CCX(a, b, t) => List(CX(a, t), CX(b, t))
      case Reset(q) => List(RZ("0", q))
      case GPhase(_) => Nil
      case NamedGate(name, params, qubits) if name.equalsIgnoreCase("rzz") && qubits.size == 2 =>
        List(CRZ(qubits(0), params.headOption.getOrElse("0"), qubits(1)))
      case NamedGate(_, _, qubits) =>
        qubits.distinct.toList match {
          case q :: Nil => List(RZ("0", q))
          case a :: b :: _ => List(CX(a, b))
          case Nil => Nil
        }
    }

  private def compilePlacedCircuit(
      circuit: Circuit,
      topology: DeviceTopology,
      gateSet: List[Gate]
  ): Either[String, Circuit] = {
    val supportedGateKinds = gateSet.iterator.map(gateKind).toSet

    def route(gate: Gate): Either[String, List[Gate]] =
      gate match {
        case two if gateQubits(two).size == 2 =>
          val qs = gateQubits(two)
          routeTwoQubitGate(two, qs(0), qs(1), topology, supportedGateKinds)
        case one if gateQubits(one).size <= 1 =>
          Right(lowerSingleQubitGate(one, supportedGateKinds))
        case other =>
          Right(List(other))
      }

    circuit.remainingGates.traverse(route).map { gates =>
      Circuit(
        remainingGates = gates.flatten,
        qubits = topology.maxPhysicalIndex + 1,
        name = if (circuit.name.nonEmpty) s"${circuit.name}_device_fit" else "device_fit_compiled"
      )
    }
  }

  private def routeTwoQubitGate(
      gate: Gate,
      a: Int,
      b: Int,
      topology: DeviceTopology,
      supportedGateKinds: Set[String]
  ): Either[String, List[Gate]] =
    topology.shortestPath(a, b, _ => 1.0) match {
      case None =>
        Left(s"No path between physical qubits $a and $b")

      case Some(path) if path.size <= 2 =>
        Right(lowerAdjacentTwoQubitGate(gate, a, b, supportedGateKinds))

      case Some(path) if gate.isInstanceOf[Swap] =>
        val endpointSwapPath =
          path.sliding(2).map(pair => pair(0) -> pair(1)).toList ++
            path.dropRight(1).sliding(2).toList.reverse.map(pair => pair(0) -> pair(1))

        Right(endpointSwapPath.flatMap { case (x, y) => lowerAdjacentSwap(x, y, supportedGateKinds) })

      case Some(path) =>
        val inwardSwaps =
          (path.indices.drop(2).reverse).map(i => path(i) -> path(i - 1)).toList
        val outwardSwaps = inwardSwaps.reverse
        val neighbor = path(1)

        Right(
          inwardSwaps.flatMap { case (x, y) => lowerAdjacentSwap(x, y, supportedGateKinds) } ++
            lowerAdjacentTwoQubitGate(gate, a, neighbor, supportedGateKinds) ++
            outwardSwaps.flatMap { case (x, y) => lowerAdjacentSwap(x, y, supportedGateKinds) }
        )
    }

  private def lowerSingleQubitGate(gate: Gate, supportedGateKinds: Set[String]): List[Gate] =
    gate match {
      case Phase(theta, q) if supportedGateKinds.nonEmpty && !supportedGateKinds.contains("phase") && supportedGateKinds.contains("rz") =>
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
      case CX(_, _) if supports("cx", supportedGateKinds) => List(CX(a, b))
      case CX(_, _) if supportedGateKinds.contains("cz") => List(H(b), CZ(a, b), H(b))
      case CZ(_, _) if supports("cz", supportedGateKinds) => List(CZ(a, b))
      case CZ(_, _) if supportedGateKinds.contains("cx") => List(H(b), CX(a, b), H(b))
      case CY(_, _) if supports("cy", supportedGateKinds) => List(CY(a, b))
      case CY(_, _) if supportedGateKinds.contains("cx") => List(SDG(b), CX(a, b), S(b))
      case CY(_, _) if supportedGateKinds.contains("cz") => List(SDG(b), H(b), CZ(a, b), H(b), S(b))
      case Swap(_, _) => lowerAdjacentSwap(a, b, supportedGateKinds)
      case CP(_, theta, _) if supports("cp", supportedGateKinds) => List(CP(a, theta, b))
      case CRX(_, theta, _) if supports("crx", supportedGateKinds) => List(CRX(a, theta, b))
      case CRY(_, theta, _) if supports("cry", supportedGateKinds) => List(CRY(a, theta, b))
      case CRZ(_, theta, _) if supports("crz", supportedGateKinds) => List(CRZ(a, theta, b))
      case CH(_, _) if supports("ch", supportedGateKinds) => List(CH(a, b))
      case CU(_, theta, phi, lambda, _) if supports("cu", supportedGateKinds) => List(CU(a, theta, phi, lambda, b))
      case NamedGate(name, params, _) => List(NamedGate(name, params, Vector(a, b)))
      case other => List(remapTwoQubitGate(other, a, b))
    }

  private def lowerAdjacentSwap(
      a: Int,
      b: Int,
      supportedGateKinds: Set[String]
  ): List[Gate] =
    if (supports("swap", supportedGateKinds)) List(Swap(a, b))
    else if (supportedGateKinds.contains("cx")) List(CX(a, b), CX(b, a), CX(a, b))
    else if (supportedGateKinds.contains("cz"))
      List(H(b), CZ(a, b), H(b), H(a), CZ(a, b), H(a), H(b), CZ(a, b), H(b))
    else List(Swap(a, b))

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

  private def effectiveGateSet(
      device: Device,
      calibration: DeviceCalibration,
      canonical: CanonicalCalibration
  ): List[Gate] = {
    val explicit = device.gateSet
    if (explicit.nonEmpty) explicit
    else {
      val calibrationKinds =
        calibration match {
          case ibm: IBMCalibration => ibm.basisGates.map(_.toLowerCase)
          case _ => Nil
        }

      val canonicalKinds =
        (canonical.eps1q.keys.map(_._2.toLowerCase) ++ canonical.eps2q.keys.map(_._2.toLowerCase)).toList

      val kinds = (calibrationKinds ++ canonicalKinds).distinct
      if (kinds.nonEmpty) kinds.flatMap(gatePrototype)
      else List(X(0), SX(0), RZ("0", 0), H(0), CX(0, 1), CZ(0, 1), Swap(0, 1), Measure(0), Reset(0))
    }
  }

  private def gatePrototype(kind: String): Option[Gate] =
    kind.toLowerCase match {
      case "x" => Some(X(0))
      case "y" => Some(Y(0))
      case "z" => Some(Z(0))
      case "h" => Some(H(0))
      case "s" => Some(S(0))
      case "sdg" => Some(SDG(0))
      case "t" => Some(T(0))
      case "tdg" => Some(TDG(0))
      case "sx" => Some(SX(0))
      case "sxdg" => Some(SXDG(0))
      case "id" => Some(Id(0))
      case "p" | "phase" => Some(Phase("0", 0))
      case "rx" => Some(RX("0", 0))
      case "ry" => Some(RY("0", 0))
      case "rz" => Some(RZ("0", 0))
      case "u" => Some(U("0", "0", "0", 0))
      case "u2" => Some(U2("0", "0", 0))
      case "u3" => Some(U3("0", "0", "0", 0))
      case "cx" | "ecr" => Some(CX(0, 1))
      case "rzz" => Some(NamedGate("rzz", Vector("0"), Vector(0, 1)))
      case "cy" => Some(CY(0, 1))
      case "cz" => Some(CZ(0, 1))
      case "ch" => Some(CH(0, 1))
      case "swap" => Some(Swap(0, 1))
      case "cp" => Some(CP(0, "0", 1))
      case "crx" => Some(CRX(0, "0", 1))
      case "cry" => Some(CRY(0, "0", 1))
      case "crz" | "crotate" => Some(CRZ(0, "0", 1))
      case "cu" => Some(CU(0, "0", "0", "0", 1))
      case "ccx" | "toffoli" => Some(CCX(0, 1, 2))
      case "measure" => Some(Measure(0))
      case "reset" => Some(Reset(0))
      case _ => None
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

  private def supports(kind: String, supportedGateKinds: Set[String]): Boolean =
    supportedGateKinds.isEmpty || supportedGateKinds.contains(kind)

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
      case CCX(a, b, t) => Vector(a, b, t)
      case Measure(q) => Vector(q)
      case Reset(q) => Vector(q)
      case GPhase(_) => Vector.empty
      case NamedGate(_, _, qubits) => qubits
    }

  private def usedQubits(circuit: Circuit): Set[Int] =
    circuit.remainingGates.iterator.flatMap(gateQubits).toSet

  private def minimumRequiredPhysicalQubits(processes: Vector[HaloCircuitMerger.ProcessCircuit]): Int =
    CircuitProcessConverter.peakLiveQubits(processes)

  private def parTraverseN[A, B](values: List[A], parallelism: Int)(f: A => IO[B]): IO[List[B]] =
    values.parTraverseN(math.max(1, parallelism))(f)

  private def formatDouble(value: Double): String =
    f"$value%.12g"

  private def csvField(value: String): String =
    if (value.exists(ch => ch == ',' || ch == '"' || ch == '\n' || ch == '\r'))
      "\"" + value.replace("\"", "\"\"") + "\""
    else value

  private def summarizeFailures(failures: Vector[DeviceFitFailure]): String =
    failures
      .groupBy(failure => failure.stage -> failure.reason)
      .toVector
      .sortBy { case (_, grouped) => -grouped.size }
      .take(5)
      .map { case ((stage, reason), grouped) =>
        s"$stage x${grouped.size}: $reason"
      }
      .mkString("; ")
}
