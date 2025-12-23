package aoc2015

import nmcb.*

import scala.util.matching.UnanchoredRegex

object Day05 extends AoC:

  val strings: List[String] = lines.toList

  val vowels: UnanchoredRegex  = "[aeiou].*[aeiou].*[aeiou]".r.unanchored
  val pair: UnanchoredRegex    = "(.)\\1".r.unanchored
  val naughty: UnanchoredRegex = "ab|cd|pq|xy".r.unanchored

  val twoPair: UnanchoredRegex = "(..).*\\1".r.unanchored
  val triple: UnanchoredRegex  = "(.).\\1".r.unanchored

  lazy val answer1: Int = strings.count(line => vowels.matches(line) && pair.matches(line) && !naughty.matches(line))
  lazy val answer2: Int = strings.count(line => twoPair.matches(line) && triple.matches(line))
