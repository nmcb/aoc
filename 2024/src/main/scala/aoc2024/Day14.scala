package aoc2024

import nmcb.*
import nmcb.pos.*
import nmcb.predef.*

import scala.annotation.*

object Day14 extends AoC:

  case class Robot(p: Pos, v: Pos)

  val space: Space =
    val robots: Vector[Robot] = lines.map:
      case s"p=$px,$py v=$vx,$vy" => Robot((px.toInt, py.toInt), (vx.toInt, vy.toInt))

    Space(robots, sizeX = 101, sizeY = 103)

  case class Space(robots: Vector[Robot], sizeX: Int, sizeY: Int):

    lazy val robotsByPos: Map[Pos,Vector[Robot]] =
      robots.groupMap(_.p)(identity)

    def render(p: Pos): String =
      robotsByPos.get(p).map(_.size.toString).getOrElse(".")

    override def toString: String =
      tiles.grouped(sizeX).map(_.map(render).mkString("")).mkString("\n")

    def tiles: IndexedSeq[Pos] =
      for
        y <- 0 until sizeY
        x <- 0 until sizeX
      yield
        (x, y)

    def move(r: Robot): Robot =
      val n = r.p + r.v
      val nx = if n.x < 0 then sizeX + n.x else if n.x >= sizeX then n.x - sizeX else n.x
      val ny = if n.y < 0 then sizeY + n.y else if n.y >= sizeY then n.y - sizeY else n.y
      Robot((nx, ny), r.v)

    def next: Space =
      copy(robots = robots.map(move))

    def safetyFactor: Long =

      def robotsIn(min: Pos, max: Pos): Vector[Robot] =
        robots.filter(r => r.p.x >= min.x & r.p.x <= max.x & r.p.y >= min.y & r.p.y <= max.y)

      val mid = (x = sizeX / 2, y = sizeY / 2)
      val robotsInQ1: Vector[Robot] = robotsIn((0, 0), (mid.x - 1, mid.y - 1))
      val robotsInQ2: Vector[Robot] = robotsIn((mid.x + 1, 0), (sizeX - 1, mid.y - 1))
      val robotsInQ3: Vector[Robot] = robotsIn((0, mid.y + 1), (mid.x - 1, sizeY - 1))
      val robotsInQ4: Vector[Robot] = robotsIn((mid.x + 1, mid.y+1), (sizeX - 1, sizeY - 1))
      Vector(robotsInQ1, robotsInQ2, robotsInQ3, robotsInQ4).map(_.size.toLong).product

    def robotClusters: Set[Set[Pos]] =

      @tailrec
      def cluster(todo: Vector[Pos], inside: Set[Pos], found: Set[Pos] = Set.empty): Set[Pos] =
        todo.runtimeChecked match
          case Vector()                        => found
          case p +: rest if inside.contains(p) => cluster(rest ++ p.adjoint4, inside - p, found + p)
          case p +: rest                       => cluster(rest, inside - p, found)

      @tailrec
      def loop(robots: Vector[Pos], inside: Set[Pos], clusters: Set[Set[Pos]] = Set.empty): Set[Set[Pos]] =
        robots.runtimeChecked match
          case Vector() =>
            clusters
          case robot +: rest =>
            val c = cluster(Vector(robot), inside)
            loop(rest, inside -- c, clusters + c)

      loop(robotsByPos.keys.toVector, robotsByPos.keySet)

  override lazy val answer1: Long =
    Iterator
      .iterate(space)(_.next)
      .nth(100)
      .safetyFactor

  override lazy val answer2: Int =
    Iterator
      .iterate(space)(_.next)
      .zipWithIndex
      .findFirst(_.element.robotClusters.exists(_.size > 50))
      .index
