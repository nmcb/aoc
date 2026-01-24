package aoc2020

import nmcb.*

import scala.annotation.tailrec

object Day15 extends AoC:

  val numbers: Vector[Int] = "5,1,9,18,13,8,0".split(',').map(_.toInt).toVector

  def solve(numbers: Vector[Int], turns: Int): Int =
    val cache = collection.mutable.Map.from(numbers.init.zipWithIndex.toMap)
    @tailrec
    def loop(index: Int, last: Int): Int =
      if index >= turns then
        last
      else
        if cache.contains(last) then
          val next = index - 1 - cache(last)
          cache += ((last, index - 1))
          loop(index + 1, next)
        else
          val next = 0
          cache += ((last, index - 1))
          loop(index + 1, next)
    loop(numbers.length, numbers.last)


  override lazy val answer1: Int = solve(numbers, 2020)
  override lazy val answer2: Int = solve(numbers, 30000000)
