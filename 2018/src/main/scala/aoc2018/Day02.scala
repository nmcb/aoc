package aoc2018

import nmcb.*
import nmcb.predef.*

object Day02 extends AoC:

  extension (identifier: String)

    private def idLettersWith(count: Int): Int =
      identifier.groupMapReduce(identity)(_ => 1)(_ + _).count(_.right == count)

    def twoCharInId: Boolean =
      idLettersWith(2) >= 1

    def threeCharInId: Boolean =
      idLettersWith(3) >= 1

  val IdCharSet: String = "abcdefghijklmnopqrstuvwxyz"

  def solve1(boxes: Vector[String]): Int =
    boxes.count(_.twoCharInId) * boxes.count(_.threeCharInId)

  def solve2(boxes: Vector[String]): String =
    def differByOneCharInPlace(box1: String, box2: String): Boolean =
      box1.zip(box2).count(_ != _) == 1

    val search =
      for
        a <- boxes
        b <- boxes
        if a != b && differByOneCharInPlace(a, b)
      yield
        a intersect b

    search.headOption.getOrElse(sys.error("unable to find"))


  override lazy val answer1: Int    = solve1(lines)
  override lazy val answer2: String = solve2(lines)
