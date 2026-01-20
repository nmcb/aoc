package aoc2022

import nmcb.*
import nmcb.pos.*

import scala.annotation.*
import scala.io.*

object Day14 extends AoC:

  enum Tile:
    case Sand
    case Air
    case Rock

  def parseLine(line: String): Vector[Pos] =
    line.trim
      .split(""" -> """)
      .foldLeft(Vector.empty[Pos]):
        case (path, s"$x,$y") => path :+ Pos.of(x.toInt,y.toInt)
        case (_, e)           => sys.error(s"unable to parse $e")


  val rocks: Vector[Pos] =

    val paths =
      Source
        .fromResource(s"$day.txt")
        .getLines
        .map(parseLine)
        .toVector

    def segment(from: Pos, to: Pos): Vector[Pos] =
      (if     from.x == to.x && from.y < to.y then (from.y to to.y).map(y => Pos.of(to.x, y))
      else if from.x == to.x && from.y > to.y then (to.y to from.y).map(y => Pos.of(to.x, y))
      else if from.y == to.y && from.x < to.x then (from.x to to.x).map(x => Pos.of(x, to.y))
      else if from.y == to.y && from.x > to.x then (to.x to from.x).map(x => Pos.of(x, to.y))
      else sys.error("boom!")).toVector

    @tailrec
    def positions(path: Vector[Pos], result: Vector[Pos] = Vector.empty): Vector[Pos] =
      path match
        case _ +: Vector()        => result
        case from +: to +: rest   => positions(to +: rest, result :++ segment(from, to))
        case _                    => sys.error("boom!")

    paths.foldLeft(Vector.empty[Pos])((rocks, path) => rocks :++ positions(path)).distinct


  case class Cave(view: Vector[Vector[Tile]], minX: Int):
    import Tile.*

    def get(p: Pos): Option[Tile] =
      view.lift(p.y).flatMap(_.lift(p.x - minX))

    private def set(p: Pos): Cave =
      Cave(view.updated(p.y, view(p.y).updated(p.x - minX, Sand)), minX)

    @tailrec
    private def land(current: Pos): (Pos, Boolean) =
      def find(p: Pos): Option[Pos] =
        val below: Vector[Pos] = Vector(Pos.of(p.x, p.y + 1), Pos.of(p.x - 1, p.y + 1), Pos.of(p.x + 1, p.y + 1))
        below.find(p => get(p).isEmpty || get(p).contains(Air))
      find(current) match
        case None                      => (current, false)
        case Some(p) if get(p).isEmpty => (p, true)
        case Some(tile)                => land(tile)

    def count: Int =
      view.flatten.count(_ == Sand)

    @tailrec
    final def solve1: Int =
      val (p, overflow) = land(Cave.drip)
      if overflow then count
      else set(p).solve1

    @tailrec
    final def solve2: Int =
      val (p, _) = land(Cave.drip)
      if p == Cave.drip then
        count + 1
      else
        set(p).solve2

  private object Cave:
    import Tile.*
    def fromRocks1(rocks: Vector[Pos]): Cave =
      val minX: Int = rocks.map(_.x).min
      val maxX: Int = rocks.map(_.x).max
      val minY: Int = 0
      val maxY: Int = rocks.map(_.y).max
      val view: Vector[Vector[Tile]] =
        val rows  = Vector.fill(maxY - minY + 1, maxX - minX + 1)(Air)
        rocks.foldLeft(rows): (row, p) =>
          row.updated(p.y, row(p.y).updated(p.x - minX, Rock))
      Cave(view, minX)

    def fromRocks2(rocks: Vector[Pos]): Cave =
      val maxY: Int = rocks.map(_.y).max
      val view: Vector[Vector[Tile]] =
        val rows  = Vector.fill(maxY + 1, 1000)(Air)
        rocks.foldLeft(rows): (row, p) =>
          row.updated(p.y, row(p.y).updated(p.x, Rock)) :+ Vector.fill(1000)(Air) :+ Vector.fill(1000)(Rock)
      Cave(view, 0)

    private val drip: Pos = Pos.of(500, 0)


  lazy val answer1: Int = Cave.fromRocks1(rocks).solve1
  lazy val answer2: Int = Cave.fromRocks2(rocks).solve2
