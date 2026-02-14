package aoc2024

import nmcb.*

object Day02 extends AoC:

  private case class Report(levels: Vector[Int]):

    private lazy val differences: Vector[Int] =
      levels
        .sliding(2)
        .collect:
          case Vector(l, r) => r - l
        .toVector

    private lazy val isIncreasing: Boolean =
      differences.forall(d => d >= 0)

    private lazy val isDecreasing: Boolean =
      differences.forall(d => d <= 0)

    private lazy val isBounded: Boolean =
      differences.forall(d => d.abs >= 1 && d.abs <= 3)

    private lazy val withOneLevelRemoved: Seq[Report] =
      levels.indices.map(i => Report(levels.take(i) ++ levels.drop(i + 1)))

    lazy val isSafe: Boolean =
      (isIncreasing || isDecreasing) && isBounded

    lazy val isSafeWithOneLevelRemoved: Boolean =
      withOneLevelRemoved.count(_.isSafe) > 0


  private val reports: Vector[Report] = lines.map(s => Report(s.split(' ').map(_.toInt).toVector))

  override lazy val answer1: Int = reports.count(_.isSafe)
  override lazy val answer2: Int = reports.count(r => r.isSafe || r.isSafeWithOneLevelRemoved)
