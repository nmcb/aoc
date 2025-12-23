package aoc2019

import nmcb.*

object Day04 extends AoC:

  def solve1(first: Int, last: Int): Int =
    (first to last).count: number =>
      def window = number.toString.map(_.asDigit).sliding(2)

      val rule1 = window.forall { case Seq(a,b) => a <= b }
      val rule2 = window.exists { case Seq(a,b) => a == b }
      rule1 && rule2
  
  def solve2(first: Int, last: Int): Int =
    (first to last).count: number =>
      def window(size: Int) = number.toString.map(_.asDigit).sliding(size)

      val rule1 = window(2).forall  { case Seq(a,b)                       => a <= b }
      val rule2 = window(2).collect { case Seq(a,b)   if a == b           => a }.toSet
      val rule3 = window(3).collect { case Seq(a,b,c) if a == b && b == c => a }.toSet
      rule1 && (rule2 -- rule3).nonEmpty

  lazy val answer1: Int = solve1(235741, 706948)
  lazy val answer2: Int = solve2(235741, 706948)
