package aoc2015

import nmcb.*

object Day17 extends AoC:

  val containers: List[Int] = lines.map(_.toInt).toList

  val fits =
    containers
      .zipWithIndex
      .toSet
      .subsets
      .map(_.toList.map(_._1))
      .filter(_.sum == 150)
      .toList

  lazy val answer1: Int =
    fits.size

  lazy val answer2: Int =
    val min = fits.map(_.size).min
    fits.count(_.size == min)