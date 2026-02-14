package aoc2016

import nmcb.*
import scala.annotation.tailrec

object Day04 extends AoC:

  case class Room(underlying: String):

    def name: String =
      underlying.takeWhile(c => !c.isDigit)

    def id: Int =
      underlying.filter(_.isDigit).toInt

    def checksum: String =
      underlying.runtimeChecked match
        case s"$nameAndId[$checksum]" => checksum

    def isValid: Boolean =

      type Chars = (Char,Int)

      extension (chars: Chars)
        def char: Char = chars._1
        def index: Int = chars._2

      given ordering: Ordering[Chars] =
        Ordering
          .by[Chars,Int](_.index)
          .reverse
          .orElse(Ordering.by[Chars,Int](_.char))

      val common: List[Char] =
        name
          .filterNot(_ == '-')
          .groupMapReduce(identity)(_ => 1)(_ + _)
          .toList
          .sorted
          .map(_.char)
          .take(5)

      checksum.forall(common.contains)

    def decrypt: String =

      @tailrec
      def rotate(c: Char, i: Int): Char =
        if c > 'z' then rotate('a', i) else if i == 0 then c else rotate((c + 1).toChar, i - 1)

      name.map:
        case '-' => ' '
        case c   => rotate(c, id)


  object Room:

    def fromLine(s: String): Room =
      Room(s)

  val rooms: Vector[Room] = lines.map(Room.fromLine)

  override lazy val answer1: Int = rooms.filter(_.isValid).map(_.id).sum
  override lazy val answer2: Int = rooms.find(_.decrypt.trim == "northpole object storage").get.id
