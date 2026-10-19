package qurator.util

import qurator.domain.Task._

object MergeSafety {
  // parentTasks is the dependency relation. childTasks can also contain result
  // partition IDs for a merged task, so it must not be interpreted as DAG edges.
  def independent(
      group: List[QuantumTask],
      knownTasks: Map[TaskId, Task] = Map.empty,
      aliases: Map[TaskId, List[TaskId]] = Map.empty
  ): Boolean = {
    val index = knownTasks ++ group.map(t => t.uuid -> t)

    def members(id: TaskId): Set[TaskId] = {
      @scala.annotation.tailrec
      def loop(todo: List[TaskId], seen: Set[TaskId], result: Set[TaskId]): Set[TaskId] =
        todo match {
          case Nil => result
          case h :: tail if seen(h) => loop(tail, seen, result)
          case h :: tail => aliases.get(h) match {
            case Some(ids) if ids.nonEmpty => loop(ids ::: tail, seen + h, result)
            case _ => loop(tail, seen + h, result + h)
          }
        }
      loop(List(id), Set.empty, Set.empty)
    }

    def parents(id: TaskId): List[TaskId] = index.get(id).toList.flatMap {
      case t: QuantumTask => t.parentTasks
      case t: ClassicalTask => t.parentTasks
      case t: SyncronizedQuantumTaskList => t.tasks.flatMap(_.parentTasks)
    }

    val memberSets = group.map(t => members(t.uuid))
    val allMembers = memberSets.flatten.toSet
    if (memberSets.exists(_.isEmpty) || allMembers.size != memberSets.map(_.size).sum) false
    else group.forall { task =>
      @scala.annotation.tailrec
      def reachesGroup(todo: List[TaskId], seen: Set[TaskId]): Boolean = todo match {
        case Nil => false
        case h :: tail if seen(h) => reachesGroup(tail, seen)
        case h :: tail =>
          val expanded = members(h)
          if (expanded.exists(allMembers)) true
          else reachesGroup(parents(h) ::: expanded.toList.flatMap(parents) ::: tail, seen + h)
      }
      !reachesGroup(parents(task.uuid) ::: members(task.uuid).toList.flatMap(parents), Set.empty)
    }
  }
}
