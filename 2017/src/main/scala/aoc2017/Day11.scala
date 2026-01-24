package aoc2017

import nmcb.*

/** @see https://www.redblobgames.com/grids/hexagons/ */
object Day11 extends AoC:

  enum Dir:
    case N
    case NE
    case SE
    case S
    case SW
    case NW

  import Dir.*

  case class Hex(x: Int, y: Int, z: Int):
    assert(x + y + z == 0)

    infix def move(dir: Dir): Hex =
      dir match
        case N  => Hex(x, y - 1, z + 1)
        case NE => Hex(x + 1, y - 1, z)
        case SE => Hex(x + 1, y, z - 1)
        case S  => Hex(x, y + 1, z - 1)
        case SW => Hex(x - 1, y + 1, z)
        case NW => Hex(x - 1, y, z + 1)

    infix def manhattanDistance(hex: Hex): Int =
      val dx = (x - hex.x).abs
      val dy = (y - hex.y).abs
      val dz = (z - hex.z).abs
      (dx + dy + dz) / 2

  object Hex:
    def zero: Hex =
      Hex(0, 0, 0)

  val path: Vector[Dir] = input.split(",").map(d => Dir.valueOf(d.toUpperCase)).toVector

  override lazy val answer1: Int = path.foldLeft(Hex.zero)(_ move _) manhattanDistance Hex.zero
  override lazy val answer2: Int = path.scanLeft(Hex.zero)(_ move _).map(_ manhattanDistance Hex.zero).max
