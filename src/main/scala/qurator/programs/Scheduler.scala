package qurator.programs

import cats.MonadThrow
import cats.effect._
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import qurator.service.DataPersistanceService
import scala.util.Random
import qurator.domain.Task._
import qurator.effects.GenUUID
import com.sinanspd.qure.circuit.Circuit

trait Scheduler[F[_]]{
    def submitTask(task: Task): F[TaskId]
}

object Scheduler{
    def make[F[_]: GenUUID: Concurrent: Logger](
        dataPersistanceService: DataPersistanceService[F],
        prioritizationStrategy: List[Task] => List[Task],
        cuttingStrategy: Circuit => List[Task],
        additionalOptimizationRuns: Circuit => List[Task]
  ): Scheduler[F] =
    new Scheduler[F] {
         def submitTask(task: Task): F[TaskId] = ???
    }
}