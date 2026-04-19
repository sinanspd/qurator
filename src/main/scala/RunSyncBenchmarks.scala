
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

object RunSyncBenchmarks extends IOApp.Simple {

    implicit val logger = Slf4jLogger.getLogger[IO]

    val syncGroups = List(
        SyncBench.SyncGroupSpec(
            tasks = List(
              QuantumTaskSpec(Circuit(List(H(0), CX(0,1)), 2), TaskQubits(2), TaskShots(1000), TaskDepth(2)),
              QuantumTaskSpec(Circuit(List(X(0), H(0)), 1), TaskQubits(1), TaskShots(1000), TaskDepth(2)),
              QuantumTaskSpec(Circuit(List(X(0), X(1), CZ(0, 1), Measure(0)), 2), TaskQubits(2), TaskShots(1000), TaskDepth(3)),
              QuantumTaskSpec(Circuit(List(X(0), H(1), Swap(0, 1), Measure(0)), 2), TaskQubits(2), TaskShots(2500), TaskDepth(3))
            ),
            coherenceBudgetMillis = 5000L
        )
    )

    def run: IO[Unit] = 
        Config.load[IO].flatMap { cfg =>
            Logger[IO].info(s"Loaded config: $cfg") *>
            Supervisor[IO].use { implicit sp =>
                AppResources
                .make[IO](cfg)
                .evalMap { res => 
                    val services = Services.make[IO](res.postgres)
                    val persistanceService = Services.make[IO](res.postgres).dataPersistanceService
                    for{
                        registry <- BenchmarkDeviceRegistry.make(
                            devices = BenchmarkDeviceRegistry.defaultDevices,
                            calibrationsById = BenchmarkDeviceRegistry.defaultCalibrations,
                            deviceEstimator = new DeviceEstimator(persistanceService)
                        )
                        dummies <- FakeBenchmarkClientsFromRegistry.make(registry)
                        clients = BenchmarkHttpClients.make(registry, dummies)
                        compiler = FakeCompiler[IO](compiled = Nil)
                        scheduler <- Scheduler.make[IO](
                            dataPersistanceService = persistanceService, 
                            clients = clients, 
                            prioritizationStrategy = (a: List[Task]) => a, 
                            cuttingStrategy = CuttingStrategies.none[IO],
                            targetEstimatedFidelity = 0.9,
                            additionalOptimizationRuns = (c: Circuit) => List(c),
                            compiler = compiler
                        )      
                        _ <- scheduler.startRuntime.use { _ => 
                            for{
                                schedRun <- SyncBench.runSchedulerSyncBenchmark(
                                                scheduler = scheduler,
                                                groups = syncGroups,
                                                registry = registry,
                                                clients = clients,
                                                compiler = compiler
                                            )
                                _ <- IO.println(
                                            s"SchedulerSync: groups/s=${schedRun.groupsPerSec}, " +
                                            s"meanStartSkew=${schedRun.meanStartSkewMillis}, " +
                                            s"meanFinishSkew=${schedRun.meanFinishSkewMillis}, " +
                                            s"meanViolation=${schedRun.meanBudgetViolationMillis}, " +
                                            s"budgetMetRate=${schedRun.budgetMetRate}, " +
                                            s"survival=${schedRun.meanSurvivalProxy}, " 
                                        )
                            }yield()
                        }
                    } yield ()
                }.useForever
            }
        }
}

