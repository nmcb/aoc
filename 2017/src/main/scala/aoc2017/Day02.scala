package aoc2017

import nmcb.*
import nmcb.predef.*

object Day02 extends AoC:

  val numberLines: Vector[Vector[Int]] =
    lines.map(_.trim.split("\\s+").map(_.toInt).toVector)

  def process(numbers: Vector[Int]): Int =
    val Vector(denominator, nominator) = numbers.combinations(2).map(_.sorted).findFirst(n => n(1) % n(0) == 0)
    nominator / denominator
    
  override lazy val answer1: Int = numberLines.map(numbers => numbers.max - numbers.min).sum
  override lazy val answer2: Int = numberLines.map(process).sum
