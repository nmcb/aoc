package aoc2023

import nmcb.*
import nmcb.pos.{*, given}

import scala.collection.mutable

/** @see Credits - https://github.com/merlinorg */
object Day23 extends AoC:
  
  val prohibited: Map[Dir, Char] =
    Map(Dir.W -> '>', Dir.N -> 'v', Dir.S -> '^', Dir.E -> '<')

  type Graph = Map[Pos, Vector[(Pos, Int)]]

  extension (grid: Grid[Char])

    def isEdge(loc: Pos): Boolean =
      loc.adjoint4.count(adj => grid.within(adj) && grid.peek(adj) != '#') > 2

    def computeEdges(from: Pos, to: Pos): Vector[(Pos, Int)] =
      val results = mutable.ListBuffer.empty[(Pos, Int)]
      def loop(p: Pos, visited: Set[Pos], length: Int): Unit =
        if p == to || isEdge(p) && p != from then
          results.addOne(p -> length)
        else if !visited.contains(p) then
          for
            d <- Dir.values
            n  = p step d
            if grid.within(n) && grid.peek(n) != '#' && grid.peek(n) != prohibited(d)
          do
            loop(n, visited + p, length + 1)
      loop(from, Set.empty, 0)
      results.toVector

    private def computeGraph(from: Pos, to: Pos): Graph =
      val result = mutable.Map.empty[Pos, Vector[(Pos, Int)]]
      def loop(p: Pos): Unit =
        val edges = computeEdges(p, to)
        result.update(p, edges)
        for
          (next, _) <- edges
          if !result.contains(next)
        do loop(next)
      loop(from)
      result.toMap

  def solve1(grid: Grid[Char]): Int =
    val from  = Pos.of(0, 1)
    val to    = Pos.of(grid.sizeX - 2, grid.sizeY - 1)
    val graph = grid.computeGraph(from, to)
    def loop(p: Pos, visited: Set[Pos], length: Int, longest: Int): Int =
      if p == to then
        length max longest
      else
        val lengths =
          for
            (next, distance) <- graph(p)
            if !visited.contains(next)
          yield
            loop(next, visited + p, length + distance, longest)
        (lengths :+ longest).max
    loop(from, Set.empty, 0, 0)

  def solve2(board: Grid[Char]): Int =
    solve1(board.map(c => if c != '#' then '.' else c))

  
  override lazy val answer1: Int = solve1(Grid.fromLines(lines))
  override lazy val answer2: Int = solve2(Grid.fromLines(lines))
