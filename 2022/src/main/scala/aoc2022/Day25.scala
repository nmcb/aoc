package aoc2022

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day25 extends AoC:

  val snafus: Vector[Number] = lines.map(Number.fromString)

  case class Digit(char: Char):

    def toLong: Long =
      char match
        case '2' =>  2L
        case '1' =>  1L
        case '0' =>  0L
        case '-' => -1L
        case '=' => -2L

    override def toString: String =
      s"${char.toString}"


  object Digit:
    val Powers: Long = 5

    def fromChar(c: Char): Digit =
      c.runtimeChecked match
        case '2' | '1' | '0' | '-' | '=' => Digit(c)

  case class Number(digits: Vector[Digit]):

    def toLong: Long =
      digits
        .reverse
        .foldLeft((0L, 1L)):
          case ((a, p), d) => (a + (p * d.toLong), 5L * p)
        .left

  object Number:

    def fromString(string: String): Number =
      @tailrec
      def loop(chars: Vector[Char], result: Vector[Digit] = Vector.empty): Number =
        chars.runtimeChecked match
          case Vector()    => Number(result)
          case h +: t => loop(t, result :+ Digit.fromChar(h))
      loop(string.toVector)

    @tailrec
    def fromLong(long: Long, result: Vector[Digit] = Vector.empty): Number =
      val remainder = long % 5

      val dividend = remainder match
        case 0 | 1 | 2 => long / 5
        case 3         => (long + 2) / 5
        case 4         => (long + 1) / 5

      val next = remainder match
        case 0 | 1 | 2 => Digit.fromChar(remainder.toString.head) +: result
        case 3         => Digit.fromChar('=') +: result
        case 4         => Digit.fromChar('-') +: result

      if dividend == 0 then Number(next) else fromLong(dividend, next)


  override lazy val answer1: String = Number.fromLong(snafus.map(_.toLong).sum).toString
