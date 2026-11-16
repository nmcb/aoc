package aoc2020

import nmcb.*
import nmcb.predef.*

object Day01 extends AoC:

  def solve(numbers: Vector[Int], numberOfArgs: Int, sumEquals: Int): Int =
    numbers
      .combinations(numberOfArgs)
      .findFirst(_.sum == sumEquals)
      .product

  val numbers: Vector[Int] =
    lines.map(_.toInt)

  override lazy val answer1: Int = solve(numbers, numberOfArgs = 2, sumEquals = 2020)
  override lazy val answer2: Int = solve(numbers, numberOfArgs = 3, sumEquals = 2020)
