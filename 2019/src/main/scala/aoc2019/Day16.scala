package aoc2019

import nmcb.*
import nmcb.predef.*

object Day16 extends AoC:

  import Iterator.*

  def solve1(signal: Seq[Int]): String =
    def phase(signal: Seq[Int]): Seq[Int] =
      signal.indices.map: digit =>
        val raw = signal.zipWithIndex.map: (next,index) =>
          ((index + 1) / (digit + 1)) % 4 match
            case 0 | 2 => 0
            case 1     => next
            case 3     => -next
        (raw.sum % 10).abs
    iterate(signal)(phase).drop(100).next.take(8).mkString

  def solve2(signal: String): String =
    def phase(signal: Seq[Int]): Seq[Int] =
      signal.scanRight(0)(_ + _).map(_ % 10)

    val index   = signal.take(7).mkString.toInt
    val initial = signal.drop(index).map(_.asDigit)
    iterate(initial)(phase).nth(100).take(8).mkString

  val signal: Seq[Int] = input.map(_.asDigit)

  lazy val answer1: String = solve1(signal)
  lazy val answer2: String = solve2(signal.mkString * 10000)
