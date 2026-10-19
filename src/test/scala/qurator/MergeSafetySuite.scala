package qurator

import cats.effect.IO
import java.time.LocalDateTime
import java.util.UUID
import qurator.domain.Task._
import qurator.domain.circuit._
import qurator.programs.Scheduler
import qurator.util.MergeSafety
import weaver.SimpleIOSuite

object MergeSafetySuite extends SimpleIOSuite {
  private def id(n: Int): TaskId = TaskId(new UUID(0L, n.toLong))
  private def task(n: Int, parents: List[TaskId] = Nil): QuantumTask =
    QuantumTask(id(n), Circuit(List(H(0), Measure(0)), 1), TaskQubits(1), TaskShots(100),
      TaskDepth(2), parents, Nil, LocalDateTime.of(2026, 1, 1, 0, 0))

  test("siblings and independent roots can share a bin") {
    val siblings = List(task(1, List(id(9))), task(2, List(id(9))))
    IO.pure(expect(MergeSafety.independent(siblings)) and
      expect(Scheduler.assignToFinalBuckets(siblings, 10, 3).size == 1) and
      expect(MergeSafety.independent(List(task(3), task(4)))))
  }

  test("direct and transitive dependencies through classical tasks prevent merging") {
    val root = task(1)
    val middle = ClassicalTask(id(2), (), List(root.uuid), Nil, root.createdAt)
    val descendant = task(3, List(middle.uuid))
    val index: Map[TaskId, Task] = Map(root.uuid -> root, middle.uuid -> middle, descendant.uuid -> descendant)
    IO.pure(expect(!MergeSafety.independent(List(root, task(4, List(root.uuid))))) and
      expect(!MergeSafety.independent(List(root, descendant), index)) and
      expect(Scheduler.assignToFinalBuckets(List(root, descendant), 10, 3, index).size == 2))
  }

  test("dependencies remain visible through previously merged aliases") {
    val a = task(1)
    val b = task(2)
    val merged = task(3)
    val descendant = task(4, List(a.uuid))
    val index: Map[TaskId, Task] = List(a, b, merged, descendant).map(t => t.uuid -> t).toMap
    val aliases = Map(merged.uuid -> List(a.uuid, b.uuid))
    IO.pure(expect(!MergeSafety.independent(List(merged, descendant), index, aliases)) and
      expect(!MergeSafety.independent(List(merged, a), index, aliases)))
  }

  test("merged result-partition IDs are not dependency edges") {
    val a = task(1)
    val merged = task(3).copy(childTasks = List(a.uuid))
    IO.pure(expect(MergeSafety.independent(List(merged, task(4)))))
  }

  test("duplicate tasks and dependency cycles are rejected without looping") {
    val a = task(1, List(id(2)))
    val b = task(2, List(id(1)))
    IO.pure(expect(!MergeSafety.independent(List(a, a))) and expect(!MergeSafety.independent(List(a, b))))
  }
}
