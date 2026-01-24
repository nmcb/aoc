package aoc2015

import nmcb.*
import nmcb.predef.*

object Day17 extends AoC:

  val containers: List[Int] = lines.map(_.toInt).toList

  val fits =
    containers
      .zipWithIndex
      .toSet
      .subsets
      .map(_.toList.map(_.element))
      .filter(_.sum == 150)
      .toList

  override lazy val answer1: Int =
    fits.size

  override lazy val answer2: Int =
    val min = fits.map(_.size).min
    fits.count(_.size == min)