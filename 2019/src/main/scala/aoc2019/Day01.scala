package aoc2019

import nmcb.*
import scala.annotation.tailrec

object Day01 extends AoC:

  val masses: Vector[Int] =lines.map(_.toInt)

  def massToFuel(mass: Int): Int =
    (mass / 3) - 2

  def massToFuelRequirement(mass: Int): Int =
    @tailrec
    def go(mass: Int, total: Int = 0): Int =
      val requirement = massToFuel(mass)
      if requirement <= 0 then
        total
      else
        go(requirement - 1, total + requirement)
    go(mass)

  lazy val answer1 = masses.map(massToFuel).sum
  lazy val answer2 = masses.map(massToFuelRequirement).sum
