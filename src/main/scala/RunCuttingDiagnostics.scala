import cats.effect._
import cats.syntax.all._
import eu.timepit.refined.auto._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import qurator.domain.Task._
import qurator.domain.circuit._
import qurator.domain.cutting._
import qurator.domain.device.Device
import qurator.testbed._
import qurator.util.FidelityEstimator
import qurator.util.HardwareAwareCuttingPlanner

object RunCuttingDiagnostics extends IOApp.Simple {

    implicit val logger = Slf4jLogger.getLogger[IO]

    private val targetEstimatedFidelity = 0.90
    private val sampleSeed = 42L
    private val sampleSize = 10

    private final case class DeviceScore(
        device: Device,
        pTotal: Double,
        logPTotal: Double
    )

    private final case class DiagnosticRow(
        index: Int,
        circuitName: String,
        qubits: Int,
        depth: Int,
        gateCount: Int,
        twoQubitGateCount: Int,
        schedulerWouldInvokeCutting: Boolean,
        dummyMeanLogFidelity: Double,
        dummyProductLogFidelity: Double,
        hardwareSelectedName: String,
        hardwareSelectedCuts: Int,
        hardwareSelectedLogFidelity: Double,
        noCutLogFidelity: Double,
        midpointGenerated: Boolean,
        midpointCuts: Option[Int],
        midpointFeasible: Option[Boolean],
        midpointScore: Option[Double],
        noCutScore: Option[Double],
        midpointViolations: List[String],
        midpointFragmentOnlyGain: Option[Double],
        midpointApparentGain: Option[Double],
        midpointClassification: Option[String],
        fastPathWouldSkip: Boolean,
        dummyWinsBenchmarkStyle: Boolean,
        flagged: Boolean,
        primaryReason: String
    )

    def run: IO[Unit] =
        for {
            loaded <- WorkloadSpecs.loadedTasks
            loadedFiltered = loaded.filter(_.qubits.value <= 5)
            specs <- WorkloadSpecs.sample(n = sampleSize, seed = sampleSeed, T = loadedFiltered)
            registry <- benchmarkRegistry(seed = sampleSeed)
            devices = benchmarkDevices(registry)
            rows <- specs.zipWithIndex.traverse { case (spec, idx) =>
                inspectCircuit(idx, spec, registry, devices)
            }
            flagged = rows.filter(_.flagged)
            _ <- IO.println(summary(rows, flagged))
            _ <- IO.println("")
            _ <- IO.println("All sampled circuits:")
            _ <- IO.println(table(rows))
            _ <- IO.println("")
            _ <- IO.println("Flagged circuits: dummy wins benchmark-style and hardware-aware selected no-cut")
            _ <- IO.println(table(flagged))
        } yield ()

    private def benchmarkDevices(registry: BenchmarkDeviceRegistry): List[Device] =
        registry.devices.map { d =>
            d.copy(queueLength = registry.queueLen(d.platformId))
        }

    private def benchmarkRegistry(seed: Long): IO[BenchmarkDeviceRegistry] = {
        val devices = BenchmarkDeviceRegistry.defaultDevices
        val byId = devices.map(d => d.platformId -> d).toMap
        val rng = new scala.util.Random(seed)
        val qMap = byId.keys.map(id => id -> rng.between(10, 200)).toMap

        for {
            fakePairs <- devices.traverse { d =>
                BenchmarkFakeDevice.make(d, qMap(d.platformId), 5L).map(fd => d.platformId -> fd)
            }
            providerJobRecordsRef <- Ref.of[IO, Map[String, JobRecord]](Map.empty)
        } yield BenchmarkDeviceRegistry(
            devicesById = byId,
            fakeDevicesById = fakePairs.toMap,
            calibrationsById = BenchmarkDeviceRegistry.defaultCalibrations,
            queueLenByDeviceId = qMap,
            providerJobRecordsRef = providerJobRecordsRef,
            msPerGate = 5L
        )
    }

    private def inspectCircuit(
        index: Int,
        spec: QuantumTaskSpec,
        registry: BenchmarkDeviceRegistry,
        devices: List[Device]
    ): IO[DiagnosticRow] = {
        val request =
            CuttingRequest(
                circuit = spec.circuit,
                devices = devices,
                targetEstimatedFidelity = targetEstimatedFidelity,
                shots = Some(spec.shots.value.toLong),
                paretoLimit = 8,
                effectiveWidthEnabled = true
            )

        val plannerConfig =
            HardwareAwareCuttingPlanner.Config(smallCircuitNoCutFastPath = false)

        val fastPathConfig =
            HardwareAwareCuttingPlanner.Config(smallCircuitNoCutFastPath = true)

        for {
            hardwareDecision <- HardwareAwareCuttingPlanner.plan[IO](
                request = request,
                fetchCalibration = d => IO.pure(registry.calibration(d.platformId)),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = plannerConfig
            )
            fastPathDecision <- HardwareAwareCuttingPlanner.plan[IO](
                request = request,
                fetchCalibration = d => IO.pure(registry.calibration(d.platformId)),
                compileCircuitFor = (_, circuit) => IO.pure(circuit),
                config = fastPathConfig
            )
            dummyDecision <- RunBenchmarks.dummyBackUpCutter(request)
            dummyScores = dummyDecision.selected.subcircuits.map(bestDeviceScore(_, registry, devices))
            dummyMeanLog = mean(dummyScores.map(_.logPTotal))
            dummyProductLog = dummyScores.map(_.logPTotal).sum
            noCutDeviceScores = devices.filter(_.qubits >= spec.qubits.value).map(scoreCircuit(_, spec.circuit, registry))
            schedulerWouldInvokeCutting = !noCutDeviceScores.exists(_.pTotal > targetEstimatedFidelity)
        } yield {
            val selected = hardwareDecision.selected
            val noCut = hardwareDecision.candidates.find(_.cutLocations.isEmpty)
            val midpoint = hardwareDecision.candidates.find(_.name == "midpoint-aggressive")
            val selectedLog =
                math.log(math.max(1e-12, selected.metrics.estimatedFidelity))
            val noCutLog =
                noCut.map(p => math.log(math.max(1e-12, p.metrics.estimatedFidelity))).getOrElse(Double.NaN)
            val dummyWins =
                dummyMeanLog > selectedLog + 1e-12
            val flagged =
                dummyWins && selected.cutLocations.isEmpty
            val fastPathWouldSkip =
                fastPathDecision.selected.name == "no-cut-small-circuit-fast-path"

            DiagnosticRow(
                index = index,
                circuitName = Option(spec.circuit.name).filter(_.nonEmpty).getOrElse(s"circuit-$index"),
                qubits = spec.qubits.value,
                depth = spec.depth.value,
                gateCount = spec.circuit.remainingGates.size,
                twoQubitGateCount = spec.circuit.remainingGates.count(g => gateQubits(g).distinct.size >= 2),
                schedulerWouldInvokeCutting = schedulerWouldInvokeCutting,
                dummyMeanLogFidelity = dummyMeanLog,
                dummyProductLogFidelity = dummyProductLog,
                hardwareSelectedName = selected.name,
                hardwareSelectedCuts = selected.cutLocations.size,
                hardwareSelectedLogFidelity = selectedLog,
                noCutLogFidelity = noCutLog,
                midpointGenerated = midpoint.isDefined,
                midpointCuts = midpoint.map(_.cutLocations.size),
                midpointFeasible = midpoint.map(_.metrics.feasible),
                midpointScore = midpoint.map(_.score),
                noCutScore = noCut.map(_.score),
                midpointViolations = midpoint.map(_.metrics.constraintViolations).getOrElse(Nil),
                midpointFragmentOnlyGain = midpoint.flatMap(_.metrics.fragmentOnlyLogGain),
                midpointApparentGain = midpoint.flatMap(_.metrics.apparentCutLogGain),
                midpointClassification = midpoint.flatMap(_.metrics.cutBenefitClassification),
                fastPathWouldSkip = fastPathWouldSkip,
                dummyWinsBenchmarkStyle = dummyWins,
                flagged = flagged,
                primaryReason = primaryReason(
                    schedulerWouldInvokeCutting = schedulerWouldInvokeCutting,
                    selected = selected,
                    midpoint = midpoint,
                    noCut = noCut,
                    fastPathWouldSkip = fastPathWouldSkip
                )
            )
        }
    }

    private def primaryReason(
        schedulerWouldInvokeCutting: Boolean,
        selected: CuttingPlan,
        midpoint: Option[CuttingPlan],
        noCut: Option[CuttingPlan],
        fastPathWouldSkip: Boolean
    ): String = {
        if (!schedulerWouldInvokeCutting) "scheduler-gate-no-cut-meets-target"
        else if (selected.name == "no-cut-small-circuit-fast-path") "smallCircuitNoCutFastPath"
        else midpoint match {
            case None =>
                if (fastPathWouldSkip) "smallCircuitNoCutFastPath-would-skip"
                else "aggressive-split-never-generated"

            case Some(plan) if !plan.metrics.feasible =>
                s"aggressive-split-rejected-constraints:${plan.metrics.constraintViolations.mkString("|")}"

            case Some(plan) =>
                val noCutScore = noCut.map(_.score).getOrElse(Double.NegativeInfinity)
                val classification = plan.metrics.cutBenefitClassification.getOrElse("unknown")
                if (selected.cutLocations.nonEmpty) s"hardware-aware-cut-selected:$classification"
                else if (plan.score <= noCutScore) s"aggressive-split-scored-below-no-cut:$classification"
                else s"no-cut-selected-despite-midpoint-score:$classification"
        }
    }

    private def bestDeviceScore(
        circuit: Circuit,
        registry: BenchmarkDeviceRegistry,
        devices: List[Device]
    ): DeviceScore =
        devices
            .filter(_.qubits >= circuit.qubits)
            .map(scoreCircuit(_, circuit, registry))
            .maxBy(_.pTotal)

    private def scoreCircuit(
        device: Device,
        circuit: Circuit,
        registry: BenchmarkDeviceRegistry
    ): DeviceScore = {
        val cal = FidelityEstimator.normalizeCalibration(registry.calibration(device.platformId))
        val est = FidelityEstimator.score(circuit, cal)
        DeviceScore(device, est.pTotal, est.logPTotal)
    }

    private def mean(xs: List[Double]): Double =
        if (xs.isEmpty) Double.NaN else xs.sum / xs.size.toDouble

    private def summary(rows: List[DiagnosticRow], flagged: List[DiagnosticRow]): String = {
        val reasons =
            flagged
                .groupBy(_.primaryReason)
                .toList
                .sortBy { case (reason, rs) => (-rs.size, reason) }
                .map { case (reason, rs) => s"$reason=${rs.size}" }
                .mkString(", ")

        List(
            s"sampleSize=${rows.size}",
            s"flagged=${flagged.size}",
            s"dummyWinsBenchmarkStyle=${rows.count(_.dummyWinsBenchmarkStyle)}",
            s"hardwareSelectedNoCut=${rows.count(_.hardwareSelectedCuts == 0)}",
            s"midpointGenerated=${rows.count(_.midpointGenerated)}",
            s"fastPathWouldSkip=${rows.count(_.fastPathWouldSkip)}",
            s"flaggedReasons=[$reasons]"
        ).mkString("\n")
    }

    private def table(rows: List[DiagnosticRow]): String = {
        val header =
            List(
                "idx",
                "circuit",
                "q",
                "depth",
                "gates",
                "2q",
                "schedInvokeCut",
                "dummyMeanLog",
                "dummyProductLog",
                "hwSelected",
                "hwCuts",
                "hwLog",
                "noCutLog",
                "midGenerated",
                "midCuts",
                "midFeasible",
                "midFragGain",
                "midAppGain",
                "midClass",
                "fastPathWouldSkip",
                "dummyWins",
                "flagged",
                "reason"
            ).mkString("\t")

        val body =
            rows.map { r =>
                List(
                    r.index.toString,
                    r.circuitName,
                    r.qubits.toString,
                    r.depth.toString,
                    r.gateCount.toString,
                    r.twoQubitGateCount.toString,
                    r.schedulerWouldInvokeCutting.toString,
                    formatDouble(r.dummyMeanLogFidelity),
                    formatDouble(r.dummyProductLogFidelity),
                    r.hardwareSelectedName,
                    r.hardwareSelectedCuts.toString,
                    formatDouble(r.hardwareSelectedLogFidelity),
                    formatDouble(r.noCutLogFidelity),
                    r.midpointGenerated.toString,
                    r.midpointCuts.map(_.toString).getOrElse(""),
                    r.midpointFeasible.map(_.toString).getOrElse(""),
                    r.midpointFragmentOnlyGain.map(formatDouble).getOrElse(""),
                    r.midpointApparentGain.map(formatDouble).getOrElse(""),
                    r.midpointClassification.getOrElse(""),
                    r.fastPathWouldSkip.toString,
                    r.dummyWinsBenchmarkStyle.toString,
                    r.flagged.toString,
                    r.primaryReason
                ).mkString("\t")
            }

        (header :: body).mkString("\n")
    }

    private def formatDouble(value: Double): String =
        if (value.isNaN) "NaN"
        else if (value.isPosInfinity) "Infinity"
        else if (value.isNegInfinity) "-Infinity"
        else f"$value%.6f"

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
}
