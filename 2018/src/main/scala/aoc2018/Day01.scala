package aoc2018

import nmcb.*
import scala.annotation.tailrec

object Day01 extends AoC:

  val frequencies: Vector[Int] = lines.map(_.toInt)


  def firstFrequencyFoundTwice(frequencies: Vector[Int]): Int =
    @tailrec
    def loop(fs: Vector[Int], seen: Set[Int] = Set(0), result: Int = 0): Int =
      fs.runtimeChecked match
        case Vector()                                  => loop(frequencies, seen, result)
        case freq +: _ if seen.contains(freq + result) => freq + result
        case freq +: rest                              => loop(rest, seen + (freq + result), freq + result)
    loop(frequencies)

  override lazy val answer1: Int = frequencies.sum
  override lazy val answer2: Int = firstFrequencyFoundTwice(frequencies)
