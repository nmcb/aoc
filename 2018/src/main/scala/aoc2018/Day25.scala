package aoc2018

import nmcb.*

object Day25 extends AoC:

  case class Dim4(w: Int, x: Int, y: Int, z: Int):
    def manhattan(that: Dim4): Int =
      (w - that.w).abs + (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

  def solve(positions: Vector[Dim4]): Int =
    positions
      .foldLeft(Set.empty[Set[Dim4]]): (constellations, pos) =>
          val (near, far) = constellations.partition(_.exists(_.manhattan(pos) <= 3))
          far + (near.flatten + pos)
      .size

  val positions: Vector[Dim4] = lines.map:
    case s"$w,$x,$y,$z" => Dim4(w.toInt, x.toInt, y.toInt, z.toInt)

  override lazy val answer1: Int    = solve(positions)
