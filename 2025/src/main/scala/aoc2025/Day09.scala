package aoc2025

import nmcb.*
import predef.*

/** credits https://www.reddit.com/r/adventofcode/comments/1phzvef/2025_day_9_part_2_python_visualization_of_the */
object Day09 extends AoC:

  type Position = (x: Long, y: Long)

  extension (pair: (Position,Position))

    def area: Long =
      val dx = (pair.left.x - pair.right.x).abs
      val dy = (pair.left.y - pair.right.y).abs
      (dx  + 1) * (dy + 1)

  val corners: Vector[Position] = lines.collect:
    case s"$x,$y" => (x = x.toLong, y = y.toLong)

  def line(p: Position, q: Position): Set[Position] =
    for
      x <- ((p.x min q.x) to (p.x max q.x)).toSet
      y <- ((p.y min q.y) to (p.y max q.y)).toSet
    yield
      (x, y)

  def perimeter(corners: Vector[Position]): Set[Position] =
    corners.zip(corners.tail :+ corners.head).flatMap(line).toSet

  def isValid(p: Position, q: Position, perimeter: Set[Position]): Boolean =
    val xMin = p.x min q.x
    val xMax = p.x max q.x
    val yMin = p.y min q.y
    val yMax = p.y max q.y
    !perimeter.exists(r => xMin < r.x && r.x < xMax && yMin < r.y && r.y < yMax)

  val perimeter: Set[Position] = perimeter(corners)
  
  given order: Ordering[(Position,Position)] = Ordering.by(_.area)

  lazy val answer1: Long = corners.pairs(using order).drain.area
  lazy val answer2: Long = corners.pairs(using order.reverse).findFirst((a,b) => isValid(a, b, perimeter)).area

