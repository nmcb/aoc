package aoc2016

import nmcb.*
import nmcb.predef.*

object Day06 extends AoC:

  type CharCount = (char: Char, count: Int)

  def solve(input: Vector[String], selector: Vector[CharCount] => Char): String =
    input
      .transpose
      .map(_.countElements.toVector)
      .map(selector)
      .mkString

  override lazy val answer1: String = solve(lines, _.maxBy(_.count).char)
  override lazy val answer2: String = solve(lines, _.minBy(_.count).char)
