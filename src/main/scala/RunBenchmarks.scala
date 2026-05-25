
import cats.effect._
import org.typelevel.log4cats.noop.NoOpLogger
import qurator.testbed._
import cats.effect.std.Supervisor
import eu.timepit.refined.auto._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import qurator.Config
import scala.language.postfixOps
import qurator.AppResources
import qurator.programs.DeviceEstimator
import qurator.clients.IBMClient
import cats.effect.Temporal
import qurator.AppResources
import qurator.clients.BraketClient
import qurator.modules.Services
import qurator.modules.HttpApi
import qurator.resources.MkHttpServer
import qurator.clients.AzureQuantumClient
import qurator.domain.IBM.SubmitJobRequestV2
import scala.annotation.meta.param
import qurator.domain.IBM.SamplerV2Input
import qurator.domain.IBM.SamplerV2PUB
import qurator.programs.Scheduler
import qurator.domain.Task._
import qurator.domain.circuit._
import qurator.util.CuttingStrategies
import qurator.domain.device._
import cats.syntax.all._

object RunBenchmarks extends IOApp.Simple {

    implicit val logger = Slf4jLogger.getLogger[IO]

    def dummyBackUpCutter: (Circuit, List[Device]) => IO[List[Circuit]] =
        (c: Circuit, _: List[Device]) => {

            val n = math.max(1, c.qubits)
            val leftWidth  = math.max(1, n / 2)
            val rightWidth = math.max(1, n - leftWidth)

            val leftRange  = 0 until leftWidth
            val rightRange = leftWidth until n

            def inLeft(q: Int): Boolean =
            leftRange.contains(q)

            def inRight(q: Int): Boolean =
            rightRange.contains(q)

            def remapLeft(g: Gate): Option[Gate] = g match {
            case X(q) if inLeft(q)           => Some(X(q))
            case H(q) if inLeft(q)           => Some(H(q))
            case SX(q) if inLeft(q)          => Some(SX(q))
            case Measure(q) if inLeft(q)     => Some(Measure(q))
            case RX(theta, q) if inLeft(q)   => Some(RX(theta, q))
            case RY(theta, q) if inLeft(q)   => Some(RY(theta, q))
            case RZ(theta, q) if inLeft(q)   => Some(RZ(theta, q))

            case CX(a, b) if inLeft(a) && inLeft(b)         => Some(CX(a, b))
            case CZ(a, b) if inLeft(a) && inLeft(b)         => Some(CZ(a, b))
            case Swap(a, b) if inLeft(a) && inLeft(b)       => Some(Swap(a, b))
            case CRZ(a, theta, b) if inLeft(a) && inLeft(b) => Some(CRZ(a, theta, b))

            case CCX(a, b, t) if inLeft(a) && inLeft(b) && inLeft(t) =>
                Some(CCX(a, b, t))

            case U(theta, phi, lambda, q) if inLeft(q) =>
                Some(U(theta, phi, lambda, q))

            case CU(ctrl, theta, phi, lambda, target)
                if inLeft(ctrl) && inLeft(target) =>
                Some(CU(ctrl, theta, phi, lambda, target))

            case _ => None
            }

            def remapRight(g: Gate): Option[Gate] = g match {
            case X(q) if inRight(q)           => Some(X(q - leftWidth))
            case H(q) if inRight(q)           => Some(H(q - leftWidth))
            case SX(q) if inRight(q)          => Some(SX(q - leftWidth))
            case Measure(q) if inRight(q)     => Some(Measure(q - leftWidth))
            case RX(theta, q) if inRight(q)   => Some(RX(theta, q - leftWidth))
            case RY(theta, q) if inRight(q)   => Some(RY(theta, q - leftWidth))
            case RZ(theta, q) if inRight(q)   => Some(RZ(theta, q - leftWidth))

            case CX(a, b) if inRight(a) && inRight(b) =>
                Some(CX(a - leftWidth, b - leftWidth))

            case CZ(a, b) if inRight(a) && inRight(b) =>
                Some(CZ(a - leftWidth, b - leftWidth))

            case Swap(a, b) if inRight(a) && inRight(b) =>
                Some(Swap(a - leftWidth, b - leftWidth))

            case CRZ(a, theta, b) if inRight(a) && inRight(b) =>
                Some(CRZ(a - leftWidth, theta, b - leftWidth))

            case CCX(a, b, t) if inRight(a) && inRight(b) && inRight(t) =>
                Some(CCX(a - leftWidth, b - leftWidth, t - leftWidth))

            case U(theta, phi, lambda, q) if inRight(q) =>
                Some(U(theta, phi, lambda, q - leftWidth))

            case CU(ctrl, theta, phi, lambda, target)
                if inRight(ctrl) && inRight(target) =>
                Some(CU(ctrl - leftWidth, theta, phi, lambda, target - leftWidth))

            case _ => None
            }

            val leftGates  = H(0) :: c.remainingGates.flatMap(remapLeft)
            val rightGates = H(0) :: c.remainingGates.flatMap(remapRight)

            val leftCircuit  = Circuit(leftGates,  leftWidth + 1, "")
            val rightCircuit = Circuit(rightGates, rightWidth + 1, "")

            List(leftCircuit, rightCircuit).pure[IO]
        }

    def run: IO[Unit] = 
        Config.load[IO].flatMap { cfg =>
            Logger[IO].info(s"Loaded config: $cfg") *>
            Supervisor[IO].use { implicit sp =>
                AppResources
                .make[IO](cfg)
                .evalMap { res => 
                    val services = Services.make[IO](res.postgres)
                    val persistanceService = Services.make[IO](res.postgres).dataPersistanceService

                    def mkEnv(seed: Long) =
                        for {
                            registry <- BenchmarkDeviceRegistry.make(
                                BenchmarkDeviceRegistry.defaultDevices,
                                BenchmarkDeviceRegistry.defaultCalibrations,
                                new DeviceEstimator(persistanceService),
                                seed = seed
                            )
                            dummies  <- FakeBenchmarkClientsFromRegistry.make(registry)
                            clients   = BenchmarkHttpClients.make(registry, dummies)
                            compiler  = FakeCompiler[IO](compiled = Nil)
                            scheduler <- Scheduler.make[IO](
                                dataPersistanceService = persistanceService,
                                clients = clients,
                                prioritizationStrategy = (a: List[Task]) => a,
                                cuttingStrategy = dummyBackUpCutter, //CuttingStrategies.cutQC[IO](cutqcClient),
                                targetEstimatedFidelity = 0.9,
                                additionalOptimizationRuns = (c: Circuit) => List(c),
                                environment = cfg.environment,
                                compiler = compiler
                            )
                        } yield (registry, clients, compiler, scheduler)

                    for{
                        loaded <- WorkloadSpecs.loadedTasks
                        loadedFiltered = loaded.filter(t => t.qubits.value <= 5) // && t.qubits.value >= 21 )
                        specs <- WorkloadSpecs.sample(n = 10, seed = 42L, T = loadedFiltered)
                        (reg1, cl1, co1, sch1) <- mkEnv(42L) //reinit so that the queue isn't tainted 
                        schedRun <- Logger[IO].info("Running Scheduler Benchmarks") *>
                            sch1.startRuntime.use(_ =>
                                SchedulerBenchmarkRunner.runSchedulerBenchmark(sch1, specs, reg1, cl1, dummyBackUpCutter, co1)
                            )

                        (reg2, cl2, co2, _) <- mkEnv(42L)
                        leastBusy <- Logger[IO].info("Running Least Busy Baselines") *>
                            SchedulerBenchmarkRunner.runBaseline(
                                SchedulerBenchmarkRunner.BaselinePolicy.LeastBusy,
                                specs,
                                reg2,
                                cl2,
                                co2
                            )

                        (reg3, cl3, co3, _) <- mkEnv(42L)
                        hiFid <- Logger[IO].info("Running Highest Fidelity Benchmarks") *>
                            SchedulerBenchmarkRunner.runBaseline(
                                SchedulerBenchmarkRunner.BaselinePolicy.HighestFidelity,
                                specs,
                                reg3,
                                cl3,
                                co3
                            )

                         _ <- IO.println(s"Scheduler: q/s=${schedRun.throughputQuantumPerSec}, meanQ=${schedRun.meanQueueWaitMillis}, meanLogF=${schedRun.meanPredictedLogFidelity}, pos_arith=${schedRun.meanPredictedSuccessProbability}, pos_geo=${schedRun.geometricMeanPredictedSuccessProbability}, n=${schedRun.uniqueSubmittedJobs}")
                         _ <- IO.println(s"LeastBusy: q/s=${leastBusy.throughputQuantumPerSec}, meanQ=${leastBusy.meanQueueWaitMillis}, meanLogF=${leastBusy.meanPredictedLogFidelity}, pos_arith=${leastBusy.meanPredictedSuccessProbability}, pos_geo=${leastBusy.geometricMeanPredictedSuccessProbability}")
                         _ <- IO.println(s"HighestF: q/s=${hiFid.throughputQuantumPerSec}, meanQ=${hiFid.meanQueueWaitMillis}, meanLogF=${hiFid.meanPredictedLogFidelity}, , pos_arith=${hiFid.meanPredictedSuccessProbability}, pos_geo=${hiFid.geometricMeanPredictedSuccessProbability}")
                    } yield ()
                }.useForever
            }
        }
}
