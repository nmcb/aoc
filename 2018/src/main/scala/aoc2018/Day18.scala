package aoc2018

import nmcb.*
import nmcb.pos.{*, given}
import nmcb.predef.*

import scala.collection.*
import scala.collection.immutable.{Map, Set}

object Day18 extends AoC:

  type Area = Map[Pos,Char]

  case class Landscape(area: Area, sizeX: Int, sizeY: Int):

    def asString: String =
      val sb = mutable.StringBuilder()
      for
        y <- 0 until sizeX
        x <- 0 until sizeX
        p = Pos.of(x,y)
      do
        sb.append(area(p))
      sb.toString.grouped(sizeX).mkString("\n","\n","\n")

    def wooded: Int =
      area.view.values.count(_ == '|')

    def lumberyards: Int =
      area.view.values.count(_ == '#')

    def resourceValue: Int =
      wooded * lumberyards

    def within(p: Pos): Boolean =
      p.x >= 0 && p.x < sizeX && p.y >= 0 && p.y < sizeY

    def surroundedBy(p: Pos, c: Char): Int =
      p.adjoint8.filter(within).count(p => area(p) == c)

    def tick: Landscape =

      val next = mutable.ArrayBuffer.empty[(Pos,Char)]
      for
        y <- 0 until sizeY
        x <- 0 until sizeX
        p = Pos.of(x,y)
      do
        val c = area(p) match
          case '.' => if surroundedBy(p, '|') >= 3 then '|' else '.'
          case '|' => if surroundedBy(p, '#') >= 3 then '#' else '|'
          case '#' => if surroundedBy(p, '#') >= 1 && surroundedBy(p, '|') >= 1 then '#' else '.'

        next += (p -> c)

      copy(area = next.toMap)

  val landscape: Landscape =
    val sizeX = lines(0).length
    val sizeY = lines.size
    val area  = List.tabulate(sizeX, sizeY)((x,y) => Pos.of(x,y) -> lines(y)(x)).flatten.toMap
    Landscape(area, sizeX, sizeY)

  override lazy val answer1: Int = Iterator.iterate(landscape)(_.tick).nth(10).resourceValue
  override lazy val answer2: Int = Cycle.find(landscape, _.tick).simulate(1000000000).resourceValue
