package aoc2015

import nmcb.*
import nmcb.predef.*

object Day10 extends AoC:

  val puzzle: String = "1113222113"

  def lookAndSay(s: String): String =
    val result = new StringBuilder
    var index  = 0
    while index < s.length do
      val char   = s(index)
      val length = 1 + s.segmentLength(_ == char, index + 1)
      result.append(length).append(char)
      index += length
    result.toString

  def solve(s: String, times: Int): Int =
    Iterator.iterate(s)(lookAndSay).nth(times).length

  lazy val answer1: Int = solve(puzzle, 40)
  lazy val answer2: Int = solve(puzzle, 50)
