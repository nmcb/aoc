package aoc2017

import nmcb.*

object Day01 extends AoC:

  val puzzle: Vector[Int] =
    input.map(_.toString.toInt).toVector

  def equal(d0: Int, d1: Int): Int = if d0 == d1 then d0 else 0

  override lazy val answer1: Int =
    puzzle.zip(puzzle.tail :+ puzzle.head).map(equal).sum

  override lazy val answer2: Int =
    val (msb, lsb) = puzzle.splitAt(puzzle.length / 2)
    puzzle.zip(lsb ++ msb).map(equal).sum
