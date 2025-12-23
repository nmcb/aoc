package aoc2018

import nmcb.*
import scala.annotation.tailrec

object Day01 extends AoC:

  val frequencies: List[Int] = lines.map(_.toInt).toList


  def firstFrequencyFoundTwice(frequencies: List[Int]): Int =
    @tailrec
    def go(fs: List[Int], seen: Set[Int] = Set(0), acc: Int = 0): Int =
      fs match
        case Nil                              => go(frequencies, seen, acc)
        case h :: _ if seen.contains(h + acc) => h + acc
        case h :: t                           => go(t, seen + (h + acc), h + acc)
    go(frequencies)

  lazy val answer1: Int = frequencies.sum
  lazy val answer2: Int = firstFrequencyFoundTwice(frequencies)
