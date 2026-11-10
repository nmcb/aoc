package aoc2022

import nmcb.*

import scala.annotation.*
import scala.math.*

object Day18 extends AoC:

  val boxes: Set[Box] =
    lines.toSet.collect:
      case s"$x,$y,$z" => Box(x.toInt, y.toInt, z.toInt)

  case class Box(x: Int, y: Int, z: Int):
    infix def -(b: Box): Box      = Box(x - b.x, y - b.y, z - b.z)
    infix def +(b: Box): Box      = Box(x + b.x, y + b.y, z + b.z)
    infix def min(b: Box): Box    = Box(x min b.x, y min b.y, z min b.z)
    infix def max(b: Box): Box    = Box(x max b.x, y max b.y, z max b.z)
    infix def >=(b: Box): Boolean = x >= b.x && y >= b.y && z >= b.z
    infix def <=(b: Box): Boolean = x <= b.x && y <= b.y && z <= b.z

    def neighbours: Set[Box] =
      val xs = Set(Box(-1,  0,  0), Box(1, 0, 0))
      val ys = Set(Box( 0, -1,  0), Box(0, 1, 0))
      val zs = Set(Box( 0,  0, -1), Box(0, 0, 1))
      (xs ++ ys ++ zs).map(this + _)

  def solve1(boxes: Set[Box]): Int =
    boxes
      .toList
      .map(p => 6 - (p.neighbours intersect boxes).size)
      .sum
  
  def solve2(boxes: Set[Box]): Int =
    val min = boxes.reduce(_ min _) - Box(1,1,1)
    val max = boxes.reduce(_ max _) + Box(1,1,1)

    @tailrec
    def flood(todo: Vector[Box], visited: Set[Box] = Set.empty): Set[Box] =
      todo.runtimeChecked match
        case Vector() => visited
        case box +: rest =>
          val reached = box.neighbours.diff(boxes).diff(visited + box).filter(box => box >= min && box <= max)
          flood(rest ++ reached, visited ++ reached + box)

    val outer = flood(todo = Vector(min))
    boxes
      .toList
      .map(_.neighbours.count(outer.contains))
      .sum

  
  override lazy val answer1: Int = solve1(boxes)
  override lazy val answer2: Int = solve2(boxes)
