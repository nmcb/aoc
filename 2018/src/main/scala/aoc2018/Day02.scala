package aoc2018

import nmcb.*

object Day02 extends AoC:

  case class Box(identifier: String):

    private def idLettersWith(count: Int): Int =
      identifier
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .count((_, occurrences) => occurrences == count)

    val twoCharInId: Boolean =
      idLettersWith(2) >= 1

    val threeCharInId: Boolean =
      idLettersWith(3) >= 1

  object Box:
    val IdCharSet: String = "abcdefghijklmnopqrstuvwxyz"
    def fromString(id: String): Box = Box(id)


  val boxes: Vector[Box] = lines.map(Box.fromString)

  def solve1(boxes: Vector[Box]): Int =
    boxes.count(_.twoCharInId) * boxes.count(_.threeCharInId)

  def differByOneCharInPlace(id1: String, id2: String): Boolean =
    id1.zip(id2).count(_ != _) == 1

  def solve2(boxes: Vector[Box]): String =
    val search =
      for
        a <- boxes
        b <- boxes
        if a != b && differByOneCharInPlace(a.identifier, b.identifier)
      yield
        a.identifier intersect b.identifier
    search.headOption.getOrElse(sys.error("unable to find"))

  lazy val answer1: Int    = solve1(boxes)
  lazy val answer2: String = solve2(boxes)
