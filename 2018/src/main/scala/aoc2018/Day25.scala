package aoc2018

import nmcb.*

object Day25 extends AoC:

  case class Dim4(x: Int, y: Int, z: Int, w: Int):
    def manhattan(that: Dim4): Int =
      (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs

  def solve(positions: Vector[Dim4]): Int =
    positions
      .foldLeft(Set.empty[Set[Dim4]]): (constellations, pos) =>
          val (near,far) = constellations.partition(_.exists(_.manhattan(pos) <= 3))
          far + (near.flatten + pos)
      .size

  val positions: Vector[Dim4] = lines.map:
    case s"$x,$y,$z,$w" => Dim4(x.toInt, y.toInt, z.toInt, w.toInt)

  lazy val answer1: Int    = solve(positions)
  lazy val answer2: String = "<unimplemented>"
