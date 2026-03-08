
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
import qurator.testcircuits.BellPairTask
import scala.annotation.meta.param
import qurator.domain.IBM.SamplerV2Input
import qurator.domain.IBM.SamplerV2PUB
import qurator.programs.Scheduler
import qurator.domain.Task._
import qurator.domain.circuit._

object RunBenchmarks extends IOApp.Simple {

    implicit val logger = Slf4jLogger.getLogger[IO]

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
                        dummies = FakeBenchmarkClientsFromRegistry.make(registry)
                        clients = BenchmarkHttpClients.make(registry, dummies)
                        compiler = FakeCompiler[IO](compiled = Nil)
                        scheduler <- Scheduler.make[IO](
                            dataPersistanceService = persistanceService, 
                            clients = clients, 
                            prioritizationStrategy = (a: List[Task]) => a, 
                            cuttingStrategy = (c: Circuit) => List(c),
                            targetEstimatedFidelity = 0.9,
                            additionalOptimizationRuns = (c: Circuit) => List(c),
                            compiler = compiler
                        )
                        specs <- WorkloadSpecs.sample(n = 2, seed = 42L, T = WorkloadSpecs.defaultT)
                        _ <- scheduler.startRuntime.use { _ => 
                            for{
                                schedRun <- SchedulerBenchmarkRunner.runSchedulerBenchmark(scheduler, specs, registry, clients, compiler)
                                leastBusy <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.LeastBusy, specs, registry, clients, compiler)
                                hiFid <- SchedulerBenchmarkRunner.runBaseline(SchedulerBenchmarkRunner.BaselinePolicy.HighestFidelity, specs, registry, clients, compiler)
        
                                _ <- IO.println(s"Scheduler: q/s=${schedRun.throughputQuantumPerSec}, meanQ=${schedRun.meanQueueWaitMillis}, meanLogF=${schedRun.meanPredictedLogFidelity}")
                                _ <- IO.println(s"LeastBusy: q/s=${leastBusy.throughputQuantumPerSec}, meanQ=${leastBusy.meanQueueWaitMillis}, meanLogF=${leastBusy.meanPredictedLogFidelity}")
                                _ <- IO.println(s"HighestF: q/s=${hiFid.throughputQuantumPerSec}, meanQ=${hiFid.meanQueueWaitMillis}, meanLogF=${hiFid.meanPredictedLogFidelity}")
                            }yield()
                        }
                    } yield ()
                }.useForever
            }
        }
}

