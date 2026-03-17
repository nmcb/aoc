package aoc2017

import nmcb.*
import nmcb.pos.*

object Day19 extends AoC:

  case class Tracer(grid: Grid[Char], pos: Pos, dir: Pos):

    def char: Char =
      grid.peekOrElse(pos, ' ')

    def isPath: Boolean =
      char != ' '

    def next: Tracer =
      grid.peekOrElse(pos + dir, ' ') match
        case '+' =>
          val turns = Pos.offset4 - dir - (-dir)
          val turn  = turns.find(turn => grid.peekOrElse(pos + dir + turn, ' ') != ' ').get
          copy(pos = pos + dir, dir = turn)
        case _ =>
          copy(pos = pos + dir, dir = dir)

    def tracePath: Vector[Char] =
      Iterator.iterate(this)(_.next).takeWhile(_.isPath).map(_.char).toVector

  object Tracer:

    def attachTo(grid: Grid[Char]): Tracer =
      Tracer(
        grid = grid,
        pos  = (x = grid.row(0).indexOf('|'), y = 0),
        dir  = (x = 0, y = 1)
      )


  val grid: Grid[Char] = Grid.fromLines(lines)

  override lazy val answer1: String = Tracer.attachTo(grid).tracePath.filter(_.isLetter).mkString("")
  override lazy val answer2: Int    = Tracer.attachTo(grid).tracePath.size
