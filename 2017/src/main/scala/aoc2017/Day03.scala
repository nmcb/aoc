package aoc2017

import nmcb.*
import nmcb.pos.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day03 extends AoC:

  def position(input: Int): Pos =
    val n    = Iterator.iterate(1)(_ + 2).findFirst(n => n * n >= input)
    val base = input - (n - 2) * (n - 2) - 1
    val size = n - 1
    val half = size / 2
    val quadrant = base / size
    val offset = base % size
    quadrant match
      case 0 => Pos.of(half, offset + 1 - half)
      case 1 => Pos.of(half - 1 - offset, half)
      case 2 => Pos.of(-half, half - 1 - offset)
      case 3 => Pos.of(offset + 1 - half, -half)

  def spiralSquares(to: Int):Int =

    val spiralScan: Seq[Pos] =
      for
        y <- -1 to 1
        x <- -1 to 1
        if !(x == 0 && y == 0)
      yield
        Pos.of(x, y)

    @tailrec
    def loop(n: Int, squares: Map[Pos,Int] = Map(Pos.of(0,0) -> 1)): Int =
      val point  = position(n)
      val result = spiralScan.map(_ + point).flatMap(squares.get).sum
      if result > to then
        result
      else
        loop(n + 1, squares.updated(point, result))
    loop(2)

  lazy val answer1: Int = position(277678).manhattan(Pos.origin).toInt
  lazy val answer2: Int = spiralSquares(277678)
