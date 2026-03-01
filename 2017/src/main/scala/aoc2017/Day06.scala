package aoc2017

import nmcb.*
import scala.annotation.tailrec

object Day06 extends AoC:

  val banks: Vector[Int] = input.split("\\s").map(_.trim.toInt).toVector

  case class Area(banks: Vector[Int]) derives CanEqual:

    extension (index: Int)
      def next: Int =
        if index + 1 >= banks.size then 0 else index + 1

    @tailrec
    final def distribute(amount: Int, index: Int, accumulator: Vector[Int]): Vector[Int] =
      if amount <= 0 then
        accumulator
      else
        val update = accumulator(index) + 1
        distribute(
          accumulator = accumulator.updated(index, update),
          index       = index.next,
          amount      = amount - 1
        )

    def redistribute: Vector[Area] =
      @tailrec
      def loop(current: Area, seen: Vector[Area] = Vector.empty): Vector[Area] =
        if seen.contains(current) then
          seen :+ current
        else
          val blocks = current.banks.max
          val index  = current.banks.indexOf(blocks)
          val bank   = current.banks.updated(index, 0)
          val next   = Area(distribute(blocks, index.next, bank))
          loop(next, seen :+ current)
      loop(this)

  def solve1(banks: Vector[Int]): Int =
    Area(banks).redistribute.size - 1

  def solve2(banks: Vector[Int]): Int =
    val seen = Area(banks).redistribute
    val last = seen.last
    seen.dropWhile(_ != last).size - 1


  override lazy val answer1: Int = solve1(banks)
  override lazy val answer2: Int = solve2(banks)
