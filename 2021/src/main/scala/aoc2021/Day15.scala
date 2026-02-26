package aoc2021

import nmcb.*
import nmcb.pos.{*, given}

import scala.math.Integral.Implicits.infixIntegralOps

object Day15 extends AoC:

  val grid: Grid[Int] = Grid.fromLines(lines).map(_.asDigit)

  def solve(grid: Grid[Int]): Int =
    Dijkstra
      .run(grid.minPos, grid.toMap)
      .distanceTo(grid.maxPos)
      .getOrElse(sys.error(s"no path from ${grid.minPos} to ${grid.maxPos}"))

  def expanded(grid: Grid[Int]): Grid[Int] =
    val matrix = Vector.tabulate(5 * grid.sizeX, 5 * grid.sizeY): (y, x) =>
      val (mx, px) = x /% grid.sizeX
      val (my, py) = y /% grid.sizeY
      val pv = grid.peek((px, py))
      1 + (pv - 1 + mx + my) % 9
    Grid.fromMatrix(matrix)

  override lazy val answer1: Int = solve(grid)
  override lazy val answer2: Int = solve(expanded(grid))
