package aoc2017

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day19 extends AoC:

  type Grid = Vector[Vector[Char]]

  val grid: Grid = lines.map(_.toVector)

  extension (g: Grid)

    def start: Pos =
      Pos.of(g.head.indexOf('|'), 0)

    def charAt(p: Pos): Char =
      grid.lift(p.y).flatMap(_.lift(p.x)).getOrElse(' ')

  case class Tracer(grid: Grid, pos: Pos, dir: Pos):

    def char: Char =
      grid.charAt(pos)

    def isPath: Boolean =
      grid.charAt(pos) != ' '

    def next: Tracer =
      grid.charAt(pos + dir) match
        case '+' =>
          val turns = Pos.offset4 - dir - (-dir)
          val turn  = turns.find(d => grid.charAt(pos + dir + d) != ' ').get
          copy(pos = pos + dir, dir = turn)
        case _ =>
          copy(pos = pos + dir, dir = dir)

  object Tracer:
    def start(grid: Grid): Tracer =
      Tracer(grid, grid.start, Pos.of(0,1))

  val tracer: Tracer    = Tracer.start(grid)
  val path: Seq[Tracer] = Iterator.iterate(tracer)(_.next).takeWhile(_.isPath).toList

  override lazy val answer1: String = path.map(_.char).filter(_.isLetter).mkString("")
  override lazy val answer2: Int    = path.size
