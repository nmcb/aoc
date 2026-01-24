package aoc2019

import nmcb.*

object Day04 extends AoC:

  def window(number: Int, size: Int): Iterator[IndexedSeq[Int]] =
    number.toString.map(_.asDigit).sliding(size)

  def solve1(first: Int, last: Int): Int =
    (first to last).count: number =>
      val rule1 = window(number, 2).forall { case Seq(a,b) => a <= b }
      val rule2 = window(number, 2).exists { case Seq(a,b) => a == b }
      rule1 && rule2
  
  def solve2(first: Int, last: Int): Int =
    (first to last).count: number =>
      val rule1 = window(number, 2).forall  { case Seq(a,b)                       => a <= b }
      val rule2 = window(number, 2).collect { case Seq(a,b)   if a == b           =>      a }.toSet
      val rule3 = window(number, 3).collect { case Seq(a,b,c) if a == b && b == c =>      a }.toSet
      rule1 && (rule2 -- rule3).nonEmpty

  override lazy val answer1: Int = solve1(235741, 706948)
  override lazy val answer2: Int = solve2(235741, 706948)
