package aoc2016

import nmcb.*
import nmcb.pos.{*, given}

import scala.util.control.Breaks._

object Day24 extends AoC:

  type Grid      = Set[Pos]
  type Nodes     = Map[Int, Pos]
  type Distances = Map[Int, Map[Int, Int]]
  type Routes    = Vector[Vector[Int]]
  
  val grid: Grid =
    val grid =
      for
        y <- lines.indices
        x <- lines.head.indices if lines(y)(x) != '#'
      yield
        (x = x, y = y)
    grid.toSet

  val nodes: Nodes =
    val nodes =
      for
        y <- lines.indices
        x <- lines.head.indices if lines(y)(x).isDigit
      yield
        lines(y)(x).asDigit -> Pos.of(x, y)
    nodes.toMap

  /** breath first search */
  def distance(grid: Grid, start: Pos, end: Pos): Int =
    import scala.collection.*
    val steps = mutable.Map(start -> 0)
    val todo  = mutable.Queue(start)

    var result: Int = 0
    breakable:
      while todo.nonEmpty do
        val current = todo.dequeue

        if current == end then
          result = steps(current)
          break()

        val update  = steps(current) + 1

        current.adjoint4.intersect(grid).foreach: next =>
          if !steps.contains(next) || update < steps(next) then
            steps(next) = update
            todo.enqueue(next)
    result

  def distances(grid: Grid, nodes: Nodes): Distances =
    nodes.transform: (_, start) =>
      nodes.transform: (_, end) =>
        distance(grid, start, end)

  /** brute force traveling salesman */
  def shortestPath(distances: Distances, routes: Routes): Int =
    routes
      .map: route =>
        route.sliding(2).map: step =>
          distances(step.head)(step.last)
        .sum
      .min

  def solve1(grid: Grid, nodes: Nodes): Int =
    val graph  = distances(grid, nodes)
    val routes = (1 to nodes.keys.max).toVector.permutations.map(0 +: _).toVector
    shortestPath(graph, routes)

  def solve2(grid: Grid, nodes: Nodes): Int =
    val graph  = distances(grid, nodes)
    val routes = (1 to nodes.keys.max).toVector.permutations.map(0 +: _ :+ 0).toVector
    shortestPath(graph, routes)


  override lazy val answer1: Int = solve1(grid, nodes)
  override lazy val answer2: Int = solve2(grid, nodes)
