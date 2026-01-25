package aoc2015

import nmcb.*
import nmcb.predef.*

object Day17 extends AoC:

  val containers: Vector[Int] = lines.map(_.toInt)

  lazy val fits: Vector[Vector[Int]] =
    containers
      .zipWithIndex
      .toSet
      .subsets
      .map(_.toVector.map(_.element))
      .filter(_.sum == 150)
      .toVector

  lazy val min: Int =
    fits.map(_.size).min


  override lazy val answer1: Int = fits.size
  override lazy val answer2: Int = fits.count(_.size == min)
