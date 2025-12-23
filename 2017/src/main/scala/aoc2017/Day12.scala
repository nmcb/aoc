package aoc2017

import nmcb.*

import scala.annotation.tailrec

object Day12 extends AoC:

  type Graph[N] = Map[N,Set[N]]

  val graph: Graph[Int] =
    lines
      .map:
        case s"$node <-> $children"
          => node.toInt -> children.trim.split(",").map(_.trim.toInt).toSet
      .toMap

  extension [N](graph: Graph[N]) def groups: Int =
    @tailrec
    def loop(todo: Graph[N] = graph, result: Int = 0): Int =
      if todo.isEmpty then
        result
      else
        val node      = todo.keys.head
        val reachable = Dijkstra.reachable(node, todo)
        loop(todo.removedAll(reachable), result + 1)
    loop()

  object Dijkstra:
    import scala.collection.*
    def reachable[N](from: N, graph: Graph[N]): immutable.Set[N] =
      val found = mutable.Set.empty[N]
      val todo  = mutable.Queue.empty[N]
      todo.enqueue(from)
      while todo.nonEmpty do
        val node = todo.dequeue
        if !found.contains(node) then
          found += node
          graph(node).iterator.foreach(todo.enqueue)
      found.toSet

  lazy val answer1: Int = Dijkstra.reachable(0, graph).size
  lazy val answer2: Int = graph.groups
