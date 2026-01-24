package aoc2017

import nmcb.*

object Day02 extends AoC:

  val puzzle: Vector[Vector[Int]] =
    lines.map(_.trim.split("\\s+").map(_.toInt).toVector)

  def process(ns: Vector[Int]): Int =
    val Vector(denominator, nominator) = ns.combinations(2).map(_.sorted).find(n => n(1) % n(0) == 0).get
    nominator / denominator

  override lazy val answer1: Int = puzzle.map(ns => ns.max - ns.min).sum
  override lazy val answer2: Int = puzzle.map(process).sum
