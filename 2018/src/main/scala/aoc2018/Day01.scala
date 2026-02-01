package aoc2018

import nmcb.*
import scala.annotation.tailrec

object Day01 extends AoC:

  val frequencies: Vector[Int] = lines.map(_.toInt)


  def firstFrequencyFoundTwice(frequencies: Vector[Int]): Int =
    @tailrec
    def go(fs: Vector[Int], seen: Set[Int] = Set(0), acc: Int = 0): Int =
      fs.runtimeChecked match
        case Vector()                         => go(frequencies, seen, acc)
        case h +: _ if seen.contains(h + acc) => h + acc
        case h +: t                           => go(t, seen + (h + acc), h + acc)
    go(frequencies)

  override lazy val answer1: Int = frequencies.sum
  override lazy val answer2: Int = firstFrequencyFoundTwice(frequencies)
