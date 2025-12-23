package aoc2016

import nmcb.*

object Day19 extends AoC:

  val puzzle = 3017957

  /** @see Credits - https://youtu.be/uCsD3ZGzMgE */
  def solve1(input: Int): Int =
    val l = input - Integer.highestOneBit(input)
    l * 2 + 1

  /** @see Credits - https://www.reddit.com/r/adventofcode/comments/5j4lp1/comment/dbdf50n/ */
  def solve2(input: Int): Int =
    var i = 1
    while i * 3 < input do i *= 3
    input - i

  lazy val answer1: Int = solve1(puzzle)
  lazy val answer2: Int = solve2(puzzle)
