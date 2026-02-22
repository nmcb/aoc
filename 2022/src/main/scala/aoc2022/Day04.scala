package aoc2022

import nmcb.*

object Day04 extends AoC:

  val sections: Vector[(Range, Range)] = lines.collect:
    case s"$l1-$r1,$l2-$r2" => (l1.toInt to r1.toInt, l2.toInt to r2.toInt)

  def contains1(r1: Range, r2: Range): Boolean =
    r1.forall(r2.contains) || r2.forall(r1.contains)

  def contains2(r1: Range, r2: Range): Boolean =
    r1.exists(r2.contains)


  override lazy val answer1: Int = sections.count(contains1)
  override lazy val answer2: Int = sections.count(contains2)
