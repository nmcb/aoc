package aoc2015

import nmcb.*

import scala.util.matching.UnanchoredRegex

object Day05 extends AoC:

  val vowels: UnanchoredRegex  = "[aeiou].*[aeiou].*[aeiou]".r.unanchored
  val pair: UnanchoredRegex    = "(.)\\1".r.unanchored
  val naughty: UnanchoredRegex = "ab|cd|pq|xy".r.unanchored

  val twoPair: UnanchoredRegex         = "(..).*\\1".r.unanchored
  val enclosedTriple: UnanchoredRegex  = "(.).\\1".r.unanchored

  def solve1(lines: Vector[String]): Int =
    lines.count: line =>
      vowels.matches(line) && pair.matches(line) && !naughty.matches(line)

  def solve2(lines: Vector[String]): Int =
    lines.count: line =>
      twoPair.matches(line) && enclosedTriple.matches(line)

  override lazy val answer1: Int = solve1(lines)
  override lazy val answer2: Int = solve2(lines)
