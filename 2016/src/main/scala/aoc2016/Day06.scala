package aoc2016

import nmcb.*
import nmcb.predef.*

object Day06 extends AoC:

  def solve(input: Vector[String], selector: Vector[(Char, Int)] => Char): String =
    input
      .transpose
      .map(_.elementCount)
      .map(selector)
      .mkString

  override lazy val answer1: String = solve(lines, _.maxBy(_.count).element)
  override lazy val answer2: String = solve(lines, _.minBy(_.count).element)
