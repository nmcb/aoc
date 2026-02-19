package aoc2024

import nmcb.*
import nmcb.predef.*

object Day19 extends AoC:

  val towels: Vector[String] =
    chunks(0).head.split(", ").toVector

  val designs: Vector[String] =
    chunks(1)

  def count(towels: Vector[String], target: String): Long =
    
    val cache = memo("" -> 1L)
    def loop(remaining: String): Long = cache.memoize(remaining):
      towels
        .filter(remaining.startsWith)
        .map(t => loop(remaining.drop(t.length)))
        .sum
    loop(target)

  override lazy val answer1: Long = designs.map(design => count(towels, design)).count(_ > 0)
  override lazy val answer2: Long = designs.map(design => count(towels, design)).sum
