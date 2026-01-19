package aoc2022

import nmcb.*
import predef.*

object Day06 extends AoC:

  def solve(input: String, size: Int): Int =
    input
      .sliding(size)
      .zipWithIndex
      .findFirst(_.element.distinct.length == size)
      .index + size


  lazy val answer1: Int = solve(input, 4)
  lazy val answer2: Int = solve(input, 14)
