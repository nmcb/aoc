package aoc2024

import nmcb.*
import pos.*

import scala.annotation.tailrec

object Day04 extends AoC:

  type Dir = Pos => Pos

  val N:  Dir = p => Pos.of(p.x, p.y - 1)
  val E:  Dir = p => Pos.of(p.x + 1, p.y)
  val S:  Dir = p => Pos.of(p.x, p.y + 1)
  val W:  Dir = p => Pos.of(p.x - 1, p.y)
  val NE: Dir = p => Pos.of(p.x + 1, p.y - 1)
  val SE: Dir = p => Pos.of(p.x + 1, p.y + 1)
  val NW: Dir = p => Pos.of(p.x - 1, p.y - 1)
  val SW: Dir = p => Pos.of(p.x - 1, p.y + 1)

  val grid: Grid[Char] = Grid.fromLines(lines)
  
  extension (grid: Grid[Char])

    @tailrec
    def read(length: Int, direction: Dir, from: Pos, result: String = ""): String =
      if length == 0 then
        result
      else
        grid.read(length - 1, direction, direction(from), result :+ grid.peekOrElse(from, '.'))

    def startsXMAS(position: Pos): Int =
      List(N, E, S, W, NE, SE, NW, SW).foldLeft(0): (count, direction) =>
        if read(4, direction, position) == "XMAS" then count + 1 else count

    def hasXMAS(position: Pos): Boolean =
      val line1 = grid.read(3, direction = SE, from = NW(position))
      val line2 = grid.read(3, direction = NE, from = SW(position))
      (line1 == "MAS" | line1 == "SAM") && (line2 == "MAS" | line2 == "SAM")


  override lazy val answer1: Int = grid.positions.foldLeft(0)((count, position) => count + grid.startsXMAS(position))
  override lazy val answer2: Int = grid.positions.count(hasXMAS(grid))
