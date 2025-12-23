package aoc2017

import nmcb.*
import scala.annotation.*

object Day19 extends AoC:

  type Grid = Vector[Vector[Char]]

  val grid: Grid = lines.map(_.toVector)

  extension (g: Grid)

    def start: Pos =
      Pos(g.head.indexOf('|'), 0)

    def charAt(p: Pos): Char =
      grid.lift(p.y).flatMap(_.lift(p.x)).getOrElse(' ')

  case class Pos(x: Int, y: Int):

    infix def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

    @targetName("-")
    def unary_- : Pos =
      Pos(-x, -y)

  object Pos:
    def offsets: Set[Pos] =
      Set(Pos(-1, 0), Pos(1, 0), Pos(0, -1), Pos(0, 1))

  case class Tracer(grid: Grid, pos: Pos, dir: Pos):

    def char: Char =
      grid.charAt(pos)

    def isPath: Boolean =
      grid.charAt(pos) != ' '

    def next: Tracer =
      grid.charAt(pos + dir) match
        case '+' =>
          val turns = Pos.offsets - dir - (-dir)
          val turn  = turns.find(d => grid.charAt(pos + dir + d) != ' ').get
          copy(pos = pos + dir, dir = turn)
        case _ =>
          copy(pos = pos + dir, dir = dir)

  object Tracer:
    def start(grid: Grid): Tracer =
      Tracer(grid, grid.start, Pos(0,1))

  val tracer: Tracer    = Tracer.start(grid)
  val path: Seq[Tracer] = Iterator.iterate(tracer)(_.next).takeWhile(_.isPath).toList

  lazy val answer1: String = path.map(_.char).filter(_.isLetter).mkString("")
  lazy val answer2: Int    = path.size
