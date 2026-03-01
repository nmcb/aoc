package aoc2018

import nmcb.*

import scala.annotation.tailrec

object Day01 extends AoC:

  val frequencies: Vector[Int] = lines.map(_.toInt)

  def firstFrequencyFoundTwice(frequencies: Vector[Int]): Int =
    @tailrec
    def loop(frequencies: Vector[Int], seen: Set[Int] = Set(0), result: Int = 0): Int =
      frequencies.runtimeChecked match
        case Vector()                            => loop(frequencies, seen, result)
        case f +: _ if seen.contains(f + result) => f + result
        case f +: rest                           => loop(rest, seen + (f + result), f + result)
    loop(frequencies)

  override lazy val answer1: Int = frequencies.sum
  override lazy val answer2: Int = firstFrequencyFoundTwice(frequencies)
