package aoc2016

import nmcb.*
import nmcb.pos.*

object Day24 extends AoC:

  type Grid      = Set[Pos]
  type Nodes     = Map[Int,Pos]
  type Distances = Map[Int,Map[Int,Int]]

  val(grid: Grid, nodes: Nodes) =
    val grid  = for y <- lines.indices ; x <- lines.head.indices if lines(y)(x) != '#'  yield Pos(x, y)
    val nodes = for y <- lines.indices ; x <- lines.head.indices if lines(y)(x).isDigit yield lines(y)(x).asDigit -> Pos(x, y)
    (grid.toSet, nodes.toMap)

  /** breath first search */
  def distance(grid: Grid, start: Pos, end: Pos): Int =
    import scala.collection.*
    val steps = mutable.Map(start -> 0)
    val todo  = mutable.Queue(start)

    while todo.nonEmpty do
      val current = todo.dequeue
      if current == end then return steps(current)
      val update  = steps(current) + 1
      current.adjoint.intersect(grid).foreach: next =>
        if !steps.contains(next) || update < steps(next) then
          steps(next) = update
          todo.enqueue(next)
    sys.error("boom!")

  def distances(grid: Grid, nodes: Nodes): Distances =
    nodes.transform: (_, start) =>
      nodes.transform: (_, end) =>
        distance(grid, start, end)

  /** brute force traveling salesman */

  type Routes = Vector[Vector[Int]]

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


  lazy val answer1: Int = solve1(grid, nodes)
  lazy val answer2: Int = solve2(grid, nodes)
