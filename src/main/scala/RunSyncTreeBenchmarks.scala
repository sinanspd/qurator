
import cats.effect._
import cats.effect.std.Supervisor
import eu.timepit.refined.auto._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import qurator.Config
import qurator.AppResources
import qurator.modules.Services
import qurator.programs.DeviceEstimator
import qurator.programs.Scheduler
import qurator.domain.Task._
import qurator.domain.circuit._
import qurator.testbed._
import qurator.util.CuttingStrategies
import scala.util.Random

object RunSyncTreeBenchmarks extends IOApp.Simple {

  implicit val logger = Slf4jLogger.getLogger[IO]

  // val syncGroups = List(
  //       SyncBench2.SyncGroupSpec(
  //           tasks = List(
  //             QuantumTaskSpec(Circuit(List(H(0), CX(0,1)), 2), TaskQubits(2), TaskShots(1000), TaskDepth(2)),
  //             QuantumTaskSpec(Circuit(List(X(0), H(0)), 1), TaskQubits(1), TaskShots(1000), TaskDepth(2)),
  //             QuantumTaskSpec(Circuit(List(X(0), X(1), CZ(0, 1), Measure(0)), 2), TaskQubits(2), TaskShots(1000), TaskDepth(3)),
  //             QuantumTaskSpec(Circuit(List(X(0), H(1), Swap(0, 1), Measure(0)), 2), TaskQubits(2), TaskShots(2500), TaskDepth(3))
  //           ),
  //           coherenceBudgetMillis = 5000L
  //       )
  //   )

  private val minQubits = 30
  private val maxQubits = 40
  private val rngSeed: Long = 42L
  private val minDepth: Int = 3
  private val maxDepth: Int = 6
  private val coherenceBudgetMillis: Long = 5000L

  private def buildTreeSpecs: IO[List[SyncBench2.RandomSyncTreeSpec]] = {
    val rng = new scala.util.Random(rngSeed)

    for {
      loaded <- WorkloadSpecs.loadedTasks
      correctSizeCandidates = loaded.filter(s => s.qubits.value >= minQubits && s.qubits.value <= maxQubits)
      pickedTasks = Random.shuffle(correctSizeCandidates.toList).take(2)
      _ <- Logger[IO].info(s"Picked tasks: ${pickedTasks.map(_.depth).mkString(", ")}")
      syncGroups = List(SyncBench2.SyncGroupSpec(pickedTasks, coherenceBudgetMillis = 5000L))
      branchQuantumPool =
        (if (loaded.nonEmpty) loaded else WorkloadSpecs.defaultT).toVector

    } yield SyncBench2.attachRandomParentsToSyncGroups(
      syncGroups = syncGroups,
      minDepth = minDepth,
      maxDepth = maxDepth,
      branchQuantumPool = branchQuantumPool,
      rng = rng
    )
  }

  def run: IO[Unit] =
    Config.load[IO].flatMap { cfg =>
      Logger[IO].info(s"Loaded config: $cfg") *>
      Supervisor[IO].use { implicit sp =>
        AppResources
          .make[IO](cfg)
          .evalMap { res =>
            val services = Services.make[IO](res.postgres)
            val persistenceService = services.dataPersistanceService

            for {
              trees <- buildTreeSpecs

              registry <- BenchmarkDeviceRegistry.make(
                devices = BenchmarkDeviceRegistry.defaultDevices,
                calibrationsById = BenchmarkDeviceRegistry.defaultCalibrations,
                deviceEstimator = new DeviceEstimator(persistenceService)
              )

              dummies <- FakeBenchmarkClientsFromRegistry.make(registry)

              clients = BenchmarkHttpClients.make(registry, dummies)
              compiler = FakeCompiler[IO](compiled = Nil)

              scheduler <- Scheduler.make[IO](
                dataPersistanceService = persistenceService,
                clients = clients,
                prioritizationStrategy = (a: List[Task]) => a,
                cuttingStrategy = CuttingStrategies.none[IO], //CuttingStrategies.cutQC[IO](cutqcClient),
                targetEstimatedFidelity = 0.7,
                additionalOptimizationRuns = (c: Circuit) => List(c),
                compiler = compiler
              )

              _ <- scheduler.startRuntime.use { _ =>
                for {
                  run <- SyncBench2.runSchedulerSyncTreeBenchmark(
                    scheduler = scheduler,
                    trees = trees,
                    registry = registry,
                    clients = clients,
                    compiler = compiler
                  )

                  _ <- IO.println(
                    s"SchedulerSyncTree: groups/s=${run.groupsPerSec}, " +
                    s"meanSyncCost=${run.meanSyncCostMillis}, " +
                    s"meanStartSkew=${run.meanStartSkewMillis}, " +
                    s"meanFinishSkew=${run.meanFinishSkewMillis}, " +
                    s"meanViolation=${run.meanBudgetViolationMillis}, " +
                    s"budgetMetRate=${run.budgetMetRate}, " +
                    s"survival=${run.meanSurvivalProxy}"
                  )
                } yield ()
              }
            } yield ()
          }
          .useForever
      }
    }
}