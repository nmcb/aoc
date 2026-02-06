package aoc2016

import nmcb.*

object Day06 extends AoC:

  type CharCount = (Char, Int)

  extension (cc: CharCount)
    def char: Char = cc._1
    def count: Int = cc._2

  type Counter = Map[Char, Int]

  extension (counter: Counter)

    infix def add(char: Char): Counter =
      counter.updatedWith(char):
        case Some(count) => Some(count + 1)
        case None        => Some(1)

    def maxChar: Char =
      counter.toVector.maxBy(_.count).char

    def minChar: Char =
      counter.toList.minBy(_.count).char

  def counted(input: Vector[String]): Vector[Counter] =
    input
      .map(_.toVector)
      .transpose
      .map(_.foldLeft(Map.empty)(_ add _))

  override lazy val answer1: String = counted(lines).map(_.maxChar).mkString
  override lazy val answer2: String = counted(lines).map(_.minChar).mkString
