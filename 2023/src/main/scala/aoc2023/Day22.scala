package aoc2023

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day22 extends AoC:

  case class Stack(stack: Vector[Box]):

    lazy val settled: Vector[Box] =
      stack
        .sortBy(_.min.z)
        .foldLeft(Vector.empty[Box]): (dropped, box) =>
          dropped :+ box.drop(
            Iterator.from(1).findFirst: height =>
              val dropping = box.drop(height)
              dropping.min.z == 0 || dropped.exists(_ intersects dropping)
            - 1
          )

    val supportedBy: Map[Box, Set[Box]] =
      settled
        .map: box =>
          box -> settled
            .filterNot(_ == box)
            .filter(_ intersects box.drop1)
            .toSet
        .toMap

    def disintegrable: Int =
      stack.toSet.size - settled
        .map: box =>
          supportedBy(box).filter(_ intersects box.drop1)
        .filter(_.size == 1)
        .map(_.head)
        .toSet
        .size

    def disintegrated: Int =

      val supports: Map[Box, Set[Box]] =
        supportedBy
          .toSet
          .flatMap: (box, foundation) =>
            foundation.map(_ -> box)
          .groupMap(_.left)(_.right)

      def disintegrate(box: Box): Int =

        @tailrec
        def loop(todo: Set[Box], found: Set[Box] = Set.empty): Int =
          if todo.isEmpty then
            found.size - 1
          else
            val box     = todo.head
            val next    = todo - box
            val support = supports.getOrElse(box, Set.empty[Box])
            val visited = found + box
            val add     = support.filter(b => (supportedBy(b) -- visited).isEmpty)
            loop(next ++ add, visited)

        loop(todo = Set(box))

      settled.map(disintegrate).sum


  val stack: Stack = Stack(lines.map(Box.fromString))

  override lazy val answer1: Int = stack.disintegrable
  override lazy val answer2: Int = stack.disintegrated

  // Geometry

  case class Vec3(x: Int, y: Int, z: Int):
    def -(that: Vec3): Vec3         = Vec3(x - that.x, y - that.y, z - that.z)
    def +(that: Vec3): Vec3         = Vec3(x + that.x, y + that.y, z + that.z)
    infix def min(that: Vec3): Vec3 = Vec3(x min that.x, y min that.y, z min that.z)
    infix def max(that: Vec3): Vec3 = Vec3(x max that.x, y max that.y, z max that.z)
    def >=(that: Vec3): Boolean     = x >= that.x && y >= that.y && z >= that.z
    def <=(that: Vec3): Boolean     = x <= that.x && y <= that.y && z <= that.z

  case class Box(min: Vec3, max: Vec3) derives CanEqual:

    def intersect(that: Box): Option[Box] =
      val maxMin = min max that.min
      val minMax = max min that.max
      if maxMin <= minMax then
        Some(Box(maxMin, minMax))
      else
        None

    infix def intersects(that: Box): Boolean =
      intersect(that).isDefined

  object Box:
    
    def fromString(s: String): Box =
      s match
        case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
          val p1 = Vec3(x1.toInt, y1.toInt, z1.toInt)
          val p2 = Vec3(x2.toInt, y2.toInt, z2.toInt)
          Box(p1, p2)

  extension (box: Box)

    def drop(height: Int): Box =
      val offset = Vec3(0, 0, height)
      Box(box.min - offset, box.max - offset)

    def drop1: Box =
      drop(1)
