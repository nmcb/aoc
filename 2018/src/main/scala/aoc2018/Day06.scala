package aoc2018

import nmcb.*
import nmcb.pos.*

import scala.collection.+:
import scala.collection.immutable.Vector
import scala.math.*

object Day06 extends AoC:

  case class Grid(coordinates: Vector[Pos]):

    val minX: Int = coordinates.minBy(_.x).x
    val maxX: Int = coordinates.maxBy(_.x).x

    val minY: Int = coordinates.minBy(_.y).y
    val maxY: Int = coordinates.maxBy(_.y).y

    val positions: Vector[Pos] =
      (for x <- minX to maxX ; y <- minY to maxY yield Pos.of(x,y)).toVector

    type UnitDistance = (Pos, Long)

    extension (unitDistance: UnitDistance)
      def coordinate: Pos = unitDistance._1
      def distance: Long  = unitDistance._2

    val closest: Vector[(Pos,Pos)] =
      positions.flatMap: p =>
        coordinates.map(c => (c, c manhattan p))
          .sortBy(_.distance).take(2) match
            case a +: b +: _ if a.distance == b.distance => None
            case      a +: _                             => Some(p,a.coordinate)
            case           _                             => sys.error(s"no distance found for position: $p")

    val areas: Map[Pos,Int] =
      closest.groupMapReduce((_,c) => c)((_,_) => 1)(_+_)

    def infinite(coordinate: Pos): Boolean =
      closest.exists((p,c) => c == coordinate && (p.x == minX || p.x == maxX || p.y == minY || p.y == maxY))

    def largestAreaSize: Long =
      val (_, size) = areas.filterNot((p,_) => infinite(p)).maxBy((_,s) => s)
      size

    def manhattanSum(position: Pos): Long =
      coordinates.map(position.manhattan).sum

    def withinManhattanSumLimit(limit: Int): Vector[Pos] =
      positions.filter(p => manhattanSum(p) < limit)


  val coordinates: Vector[Pos] = lines.map:
    case s"$x, $y" => Pos.of(x.toInt, y.toInt)

  lazy val answer1: Long = Grid(coordinates).largestAreaSize
  lazy val answer2: Int  = Grid(coordinates).withinManhattanSumLimit(10000).size
