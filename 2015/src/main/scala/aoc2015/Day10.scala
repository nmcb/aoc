package aoc2015

import nmcb.*
import nmcb.predef.*

object Day10 extends AoC:

  override lazy val input: String = "1113222113"

  def lookAndSay(number: String): String =
    val result = new StringBuilder
    var index  = 0
    while index < number.length do
      val char   = number(index)
      val length = 1 + number.segmentLength(_ == char, index + 1)
      result.append(length).append(char)
      index += length
    result.toString

  def solve(s: String, times: Int): Int =
    Iterator.iterate(s)(lookAndSay).nth(times).length

  override lazy val answer1: Int = solve(input, 40)
  override lazy val answer2: Int = solve(input, 50)
