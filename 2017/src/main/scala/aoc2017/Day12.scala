package aoc2017

import nmcb.*

import scala.annotation.tailrec

object Day12 extends AoC:

  type Graph[N] = Map[N, Set[N]]

  val graph: Graph[Int] =
    lines
      .collect:
        case s"$node <-> $children" => node.toInt -> children.trim.split(",").map(_.trim.toInt).toSet
      .toMap

  extension [N](graph: Graph[N])

    def groups: Int =
      @tailrec
      def loop(todo: Graph[N] = graph, result: Int = 0): Int =
        if todo.isEmpty then
          result
        else
          val node      = todo.keys.head
          val reachable = Dijkstra.reachable(node, todo)
          loop(todo.removedAll(reachable), result + 1)
      loop()

  override lazy val answer1: Int = Dijkstra.reachable(0, graph).size
  override lazy val answer2: Int = graph.groups
