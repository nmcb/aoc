package aoc2018

import nmcb.*

object Day25 extends AoC:

  case class Pos(x: Int, y: Int, z: Int, w: Int):
    def manhattan(that: Pos): Int =
      (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs

  def solve(positions: Vector[Pos]): Int =
    positions
      .foldLeft(Set.empty[Set[Pos]]): (constellations,pos) =>
          val (near,far) = constellations.partition(_.exists(_.manhattan(pos) <= 3))
          far + (near.flatten + pos)
      .size

  val positions: Vector[Pos] = lines.map:
    case s"$x,$y,$z,$w" => Pos(x.toInt, y.toInt, z.toInt, w.toInt)

  lazy val answer1: Int    = solve(positions)
  lazy val answer2: String = "<unimplemented>"
