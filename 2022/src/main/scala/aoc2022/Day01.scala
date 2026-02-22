package aoc2022

import nmcb.*

object Day01 extends AoC:

  val calories: Vector[Vector[Int]] = chunks.map(_.map(_.toInt))


  override lazy val answer1: Int = calories.map(_.sum).max
  override lazy val answer2: Int = calories.map(_.sum).sorted.takeRight(3).sum
