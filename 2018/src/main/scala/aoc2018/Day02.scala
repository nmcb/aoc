package aoc2018

import nmcb.*
import nmcb.predef.*

object Day02 extends AoC:

  extension (box: String)

    private def sameLetterCount(nr: Int): Int =
      box.countElements.count(_.right == nr)

    def hasTwoChars: Boolean =
      sameLetterCount(nr = 2) >= 1

    def hasThreeChars: Boolean =
      sameLetterCount(nr = 3) >= 1

    def differByOneCharInPlace(that: String): Boolean =
      box.zip(that).count(_ != _) == 1


  def solve1(boxes: Vector[String]): Int =
    boxes.count(_.hasTwoChars) * boxes.count(_.hasThreeChars)

  def solve2(boxes: Vector[String]): String =
    val found = for
      a <- boxes
      b <- boxes
      if a != b && a.differByOneCharInPlace(b)
    yield
      a intersect b
    found.head


  override lazy val answer1: Int    = solve1(lines)
  override lazy val answer2: String = solve2(lines)
