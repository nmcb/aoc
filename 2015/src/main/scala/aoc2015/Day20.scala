package aoc2015

import nmcb.*

object Day20 extends AoC:

  val puzzle: Int = 36000000

  lazy val answer1: Int =
    val presents: Array[Int] = Array.fill(puzzle / 10 + 1)(0) ; for {
      e <- Range.inclusive(1, puzzle / 10)
      h <- Range.inclusive(e, puzzle / 10, e)
    } yield presents(h) = presents(h) + e * 10

    presents
      .zipWithIndex
      .find((count,_) => count >= puzzle)
      .getOrElse(sys.error("not found"))
      ._2

  lazy val answer2: Int =
    val size = puzzle / 10 + 1
    val presents: Array[Int] = Array.fill(size)(0) ; for {
      e <- Range.inclusive(1, size)
      h <- Range.inclusive(e, e * 50, e)
      if h < size
    } yield presents(h) = presents(h) + e * 11

    presents
      .zipWithIndex
      .find((count,_) => count >= puzzle)
      .getOrElse(sys.error("not found"))
      ._2
