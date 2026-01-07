package aoc2019

import nmcb.*
import nmcb.pos.*

object Day03 extends AoC:

  val Vector(description1, description2) = lines

  def append(wire: Vector[Pos], cmd: String): Vector[Pos] =

    val length: Int = cmd.drop(1).toInt
    val next: Pos   = wire.lastOption.getOrElse(Pos.origin)

    cmd.head match
      case 'R' => wire ++ (1 to length).map(i => Pos.of(next.x + i, next.y))
      case 'L' => wire ++ (1 to length).map(i => Pos.of(next.x - i, next.y))
      case 'U' => wire ++ (1 to length).map(i => Pos.of(next.x, next.y + i))
      case 'D' => wire ++ (1 to length).map(i => Pos.of(next.x, next.y - i))

  def wire(description: String): Vector[Pos] =
    description.split(',').foldLeft(Vector.empty)((wire,cmd) => append(wire, cmd))
  
  def distances(description1: String, description2: String): Vector[Pos] =
    wire(description1).toSet.intersect(wire(description2).toSet).toVector.sorted(using Pos.orderingByManhattanDistance)
  
  type Crossing = (Pos,Int)

  extension (crossing: Crossing)
    def pos: Pos   = crossing._1
    def count: Int = crossing._2

  def minCrossingsOnIntersection(crossings1: Vector[Crossing], crossings2: Vector[Crossing]): Crossing =
    val intersections = crossings1.map(_.pos).toSet.intersect(crossings2.map(_.pos).toSet).toVector
    val crossingCount1 = crossings1.filter(crossing => intersections.contains(crossing.pos)).toMap
    val crossingCount2 = crossings2.filter(crossing => intersections.contains(crossing.pos)).toMap

    intersections
      .map(pos => pos -> (crossingCount1(pos) + crossingCount2(pos) + 2))
      .sortWith(_.count < _.count)
      .head

  def solve1(description1: String, description2: String): Int =
    distances(description1, description2).head.manhattan(Pos.origin).toInt

  def solve2(description1: String, description2: String): Int =
    val crossings1 = wire(description1).zipWithIndex
    val crossings2 = wire(description2).zipWithIndex
    minCrossingsOnIntersection(crossings1,crossings2).count


  lazy val answer1: Int = solve1(description1, description2)
  lazy val answer2: Int = solve2(description1, description2)
