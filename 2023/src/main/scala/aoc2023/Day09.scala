package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day09 extends AoC:

  lazy val matrix: Vector[Vector[Int]] = lines.map(_.split(' ').map(_.toInt).toVector)

  def extrapolate(line: Vector[Int]): Int =
    @tailrec
    def loop(current: Vector[Int], result: Int): Int =
      val diffs = current.tail.zip(current).map(_ - _)
      if diffs.forall(_ == 0) then result + current.last else loop(diffs, result + current.last)
    loop(line, 0)

  override lazy val answer1: Int = matrix.map(extrapolate).sum
  override lazy val answer2: Int = matrix.map(_.reverse).map(extrapolate).sum
