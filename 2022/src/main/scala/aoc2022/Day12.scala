package aoc2022

import nmcb.*
import nmcb.predef.*
import nmcb.pos.{*, given}

object Day12 extends AoC:

  import Dijkstra.*

  val grid: Grid[Char] = Grid.fromLines(lines)
  val from: Pos        = grid.findOne('S')
  val to: Pos          = grid.findOne('E')

  def height(c: Char): Int =
    if      c == 'S' then 0
    else if c == 'E' then 25
    else                  c.toInt - 97

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


  val graph: Graph[Pos] = grid.toGraph
  val starts: Set[Pos]  = grid.filter(p => p.right == 'a' || p.right == 'S').map(_.left)

  override lazy val answer1: Int = solve(from, to, graph).getOrElse(sys.error(s"no path from $from to $to"))
  override lazy val answer2: Int = starts.flatMap(from => solve(from, to, graph)).min
