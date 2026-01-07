package aoc2018

import nmcb.*
import nmcb.pos.*

object Day17 extends AoC:

  extension (p: Pos)
    def e: Pos = (x = p.x - 1, y = p.y)
    def w: Pos = (x = p.x + 1, y = p.y)
    def s: Pos = (x = p.x, y = p.y + 1)

  case class Area(clay: Set[Pos], minY: Int, maxY: Int, spring: Pos)

  enum Stream:
    case Stopped
    case Flowing

  import Stream.*

  import Iterator.*

  /** return all flowing and stopped positions */
  def stream(area: Area): (Int,Int) =
    val stopped = collection.mutable.Set[Pos]()
    val flowing = collection.mutable.Set[Pos]()

    def blocked(p: Pos) = stopped.contains(p) || area.clay.contains(p)

    def go(p: Pos): Stream =
      if blocked(p) then
        Stopped
      else if p.y > area.maxY || flowing.contains(p) then
        Flowing
      else go(p.s) match
        case Flowing =>
          flowing += p
          Flowing
        case Stopped =>
          val minX = iterate(p)(_.e).dropWhile(next => go(next.s) == Stopped && !blocked(next.e)).next
          val maxX = iterate(p)(_.w).dropWhile(next => go(next.s) == Stopped && !blocked(next.w)).next
          val surface = for x <- minX.x to maxX.x yield Pos.of(x, p.y)
          if blocked(minX.e) && blocked(maxX.w) then
            stopped ++= surface
            Stopped
          else
            flowing ++= surface
            Flowing

    go(area.spring)
    (flowing.count(p => p.y >= area.minY), stopped.size)


  val area: Area =
    val clay =
      lines
        .flatMap:
          case s"x=$x, y=$start..$end" => (start.toInt to end.toInt).map(y => Pos.of(x.toInt, y))
          case s"y=$y, x=$start..$end" => (start.toInt to end.toInt).map(x => Pos.of(x, y.toInt))
        .toSet

    Area(
      clay   = clay,
      minY   = clay.map(_.y).min,
      maxY   = clay.map(_.y).max,
      spring = Pos.of(500,0)
    )
  
  def solve1(area: Area): Int =
    val (flowing, stopped) = stream(area)
    flowing + stopped

  def solve2(area: Area): Int =
    val (flowing, stopped) = stream(area)
    stopped

  lazy val answer1: Int = solve1(area)
  lazy val answer2: Int = solve2(area)
