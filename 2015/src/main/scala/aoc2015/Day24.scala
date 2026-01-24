package aoc2015

import nmcb.*

object Day24 extends AoC:

  val weights: Vector[Long] = lines.map(_.toLong)

  def solve(weights: Vector[Long], groups: Int): Long =
    val balance = weights.sum / groups
    Iterator
      .from(1)                          // We increment from 1 so it's the minimum
      .flatMap: configuration =>        // configuration for one group, then we
        weights                         // compute all possible weight combinations
          .combinations(configuration)  // for that configuration and filter out
          .filter(_.sum == balance)     // combinations that sum to balance assuming
          .map(_.product)               // all other groups can be balanced, finally
      .next                             // we calculate the quantum entanglement.

  override lazy val answer1: Long = solve(weights, 3)
  override lazy val answer2: Long = solve(weights, 4)
