package aoc2015

import nmcb.*

import scala.util.matching.UnanchoredRegex

object Day05 extends AoC:

  val vowels: UnanchoredRegex  = "[aeiou].*[aeiou].*[aeiou]".r.unanchored
  val pair: UnanchoredRegex    = "(.)\\1".r.unanchored
  val naughty: UnanchoredRegex = "ab|cd|pq|xy".r.unanchored

  val twoPair: UnanchoredRegex = "(..).*\\1".r.unanchored
  val triple: UnanchoredRegex  = "(.).\\1".r.unanchored

  override lazy val answer1: Int = lines.count(line => vowels.matches(line) && pair.matches(line) && !naughty.matches(line))
  override lazy val answer2: Int = lines.count(line => twoPair.matches(line) && triple.matches(line))
