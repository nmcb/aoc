package aoc2019

import nmcb.*
import scala.annotation.tailrec

object Day19 extends AoC:

  import cpu.*

  def pulls(program: Mem, xs: Range, ys: Range): IndexedSeq[Value] =
    for
      y <- ys
      x <- xs
    yield
      CPU(program).withInput(x,y).outputs.last

  def wiggleSouthEast(program: Mem, maxX: Value, maxY: Value): Long =

    def isPulled(x: Value, y: Value): Boolean =
      CPU(program).withInput(x,y).outputs.head == 1

    @tailrec
    def go(x: Value, y: Value): Value =
      val ne = isPulled(x, y - maxY)
      val sw = isPulled(x - maxX, y)

      if ne && sw then
        10000 * (x - maxX) + (y - maxY)
      else if ne then
        go(x + 1, y)
      else
        go(x, y + 1)

    go(maxX, maxY)

  val program: Mem = Mem.parse(input)

  lazy val answer1: Int  = pulls(program, 0 until 50, 0 until 50).count(_ == 1)
  lazy val answer2: Long = wiggleSouthEast(program, maxX = 99, maxY = 99)
