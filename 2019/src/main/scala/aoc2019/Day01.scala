package aoc2019

import nmcb.*

import scala.annotation.tailrec

object Day01 extends AoC:

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

  val masses: Vector[Int] = lines.map(_.toInt)
  
  override lazy val answer1: Int = masses.map(massToFuel).sum
  override lazy val answer2: Int = masses.map(massToFuelRequirement).sum
