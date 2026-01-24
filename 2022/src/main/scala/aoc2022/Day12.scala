package aoc2022

import nmcb.*
import nmcb.pos.*

object Day12 extends AoC:

  import Dijkstra.*

  lazy val (grid, from, to): (Grid[Char], Pos, Pos) =
    val puzzle: Grid[Char] = Grid.fromLines(lines)
    (puzzle, puzzle.findOne('S'), puzzle.findOne('E'))

  def height(c: Char): Int =
    if c == 'S' then 0
    else if c == 'E' then 25
    else c.toInt - 97

  def weight(f: Pos, t: Pos): Option[Int] =
    val fh = height(grid.peek(f))
    val th = height(grid.peek(t))
    th.compare(fh) match
      case 1 => // th > fh
        if th - fh == 1 then Some(2) else None
      case 0 => // th = fh
        Some(1)
      case -1 => // th < fh
        Some(th - fh + 2)

  extension (grid: Grid[Char])
    def toGraph: Graph[Pos] =
      Graph.fromEdges:
        for
          from   <- grid.positions.toVector
          to     <- from.adjoint4.filter(grid.within)
          weight <- weight(from, to)
        yield
          Edge(from, to, weight)

  def solve(from: Pos, to: Pos, graph: Graph[Pos]): Option[Int] =
    val path = Dijkstra.run(from, graph).pathTo(to)
    Option.when(path.nonEmpty)(path.length)


  override lazy val answer1: Int = solve(from, to, grid.toGraph).getOrElse(sys.error(s"no path from $from to $to"))
  override lazy val answer2: Int = grid.filter((_,c) => c == 'a' || c == 'S').flatMap((f,_) => solve(f, to, grid.toGraph)).min

