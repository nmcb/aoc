package aoc2018

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day12 extends AoC:

  type Pattern  = String
  type PotIndex = Int
  type Rules    = Set[Pattern]
  type Plants   = Set[PotIndex]

  val (rules: Rules, plants: Plants) =

    /** note that we only collect pot indices that contain a plant */
    val plants = lines(0) match
      case s"initial state: $pots" => pots.zipWithIndex.filter(_.element == '#').map(_.index).toSet

    /** note that we only collect rules that yield a plant */
    val rules = lines.drop(2).toSet.collect:
      case s"$pattern => $output" if output == "#" => pattern

    (rules, plants)

  extension (plants: Set[Int])

    /** returns the relevant surrounding pattern for given pot index */
    def pattern(index: PotIndex): String =
      (-2 to 2).map(i => if plants.contains(index + i) then '#' else '.').mkString

    /** returns the relevant range of plant containing pots */
    def pots: Range =
      plants.min - 2 to plants.max + 2

    /** returns the next generation of plant containing pot indices from that range for given rules */
    def next(rules: Rules): Set[Int] =
      pots.flatMap(pot => Option.when(rules.contains(pattern(pot)))(pot)).toSet

  def solve(plants: Plants, rules: Rules, generations: Long): Long =

    /** utilises the observation that the problem converses linearly from generation 102 and on */
    if generations >= 102 then
      @tailrec
      def go(plants: Plants, conversion: Int, generation: Int): Long =
        val next  = plants.next(rules)
        val delta = next.sum - plants.sum
        if delta == conversion then
          next.sum + delta * (generations - generation)
        else
          go(next, delta, generation + 1)
      go(plants, 0, 1)
    else
      Iterator.iterate(plants)(_.next(rules)).nth(generations.toInt).sum.toLong

  lazy val answer1: Long = solve(plants, rules, generations = 20)
  lazy val answer2: Long = solve(plants, rules, generations = 50000000000L)
