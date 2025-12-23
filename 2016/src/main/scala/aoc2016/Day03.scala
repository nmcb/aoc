package aoc2016

import nmcb.*

import scala.annotation.tailrec

object Day03 extends AoC:

  type Triangle = Vector[Int]

  extension (triangle: Triangle)
    def a: Int = triangle(0)
    def b: Int = triangle(1)
    def c: Int = triangle(2)

    def isValid: Boolean = triangle.permutations.forall(t => t.a + t.b > t.c)

  object Triangle:

    def fromLine(s: String): Vector[Int] =
      val sides = s.trim.split("\\s+")
      Vector(sides(0).toInt, sides(1).toInt, sides(2).toInt)

  val triangles: Vector[Triangle] = lines.map(Triangle.fromLine)

  @tailrec
  def triangulate(todo: Vector[Int], result: Vector[Triangle] = Vector.empty): Vector[Triangle] =
    todo match
      case a +: b +: c +: rest => triangulate(rest, result :+ Vector(a, b, c))
      case _                   => result

  lazy val answer1: Int = triangles.count(_.isValid)
  lazy val answer2: Int = triangulate(triangles.transpose.flatten).count(_.isValid)
