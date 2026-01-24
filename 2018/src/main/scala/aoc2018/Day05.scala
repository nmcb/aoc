package aoc2018

import nmcb.*

object Day05 extends AoC:

  def opposites(a: Char, b: Char): Boolean =
    a != b && a.toLower == b.toLower

  def react(poly: String): String =
    poly
      .foldLeft(List.empty[Char]):
        case (a :: it, b) if opposites(a, b) => it
        case (init, b)                       => b :: init
      .reverse
      .mkString("")

  def solve1(poly: String): Int =
    react(poly).length

  def solve2(poly: String): Int =
    val reacted = react(poly)
    val units   = reacted.toLowerCase.toSet
    units.map(unit => reacted.filterNot(_.toLower == unit)).map(solve1).min

  override lazy val answer1: Int = solve1(input)
  override lazy val answer2: Int = solve2(input)
