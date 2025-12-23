package aoc2017

import nmcb.*

object Day02 extends AoC:

  val puzzle: Seq[Seq[Int]] = lines.map(_.trim.split("\\s+").map(_.toInt).toSeq)

  def process(ns: Seq[Int]): Int =
    val Seq(denominator, nominator) = ns.combinations(2).map(_.sorted).find(n => n(1) % n(0) == 0).get
    nominator / denominator

  lazy val answer1: Int = puzzle.map(ns => ns.max - ns.min).sum
  lazy val answer2: Int = puzzle.map(process).sum
