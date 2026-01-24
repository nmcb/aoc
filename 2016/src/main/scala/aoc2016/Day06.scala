package aoc2016

import nmcb.*

object Day06 extends AoC:

  type CharCount = (Char,Int)

  extension (cc: CharCount)
    def char: Char = cc._1
    def count: Int = cc._2

  case class Counter(counts: Map[Char,Int] = Map.empty):

    infix def add(char: Char): Counter =
      Counter(
        counts.updatedWith(char):
          case Some(count) => Some(count + 1)
          case None        => Some(1)
      )

    def max: Char =
      counts.toVector.maxBy(_.count).char

    def min: Char =
      counts.toList.minBy(_.count).char

  object Counter:

    val empty: Counter =
      Counter()

  def counted(input: Vector[String]): Vector[Counter] =
    input
      .map(_.toVector)
      .transpose
      .map(_.foldLeft(Counter.empty)(_ add _))

  override lazy val answer1: String = counted(lines).map(_.max).mkString
  override lazy val answer2: String = counted(lines).map(_.min).mkString
