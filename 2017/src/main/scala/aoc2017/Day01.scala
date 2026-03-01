package aoc2017

import nmcb.*

object Day01 extends AoC:

  val digits: Vector[Int] = input.map(_.asDigit).toVector

  def getIfEqualElseZero(d0: Int, d1: Int): Int =
    if d0 == d1 then d0 else 0

  def solve1(digits: Vector[Int]): Int =
    digits.zip(digits.tail :+ digits.head).map(getIfEqualElseZero).sum

  def solve2(digits: Vector[Int]): Int =
    val (msb, lsb) = digits.splitAt(digits.length / 2)
    digits.zip(lsb ++ msb).map(getIfEqualElseZero).sum

  
  override lazy val answer1: Int = solve1(digits)
  override lazy val answer2: Int = solve2(digits)
  