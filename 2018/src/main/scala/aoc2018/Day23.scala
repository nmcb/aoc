package aoc2018

import nmcb.*
import nmcb.BronKerbosch.maximumClique

object Day23 extends AoC:

  case class Location(x: Int, y: Int, z: Int):
    infix def manhattan(that: Location): Int =
      (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

  object Location:
    val origin: Location = Location(0, 0, 0)

  case class Bot(loc: Location, radius: Int) derives CanEqual:
    infix def overlapsWith(that: Bot): Boolean =
      loc.manhattan(that.loc) <= radius + that.radius

  val bots: Set[Bot] =
    lines
      .collect:
        case s"pos=<$x,$y,$z>, r=$r" => Bot(Location(x.toInt, y.toInt, z.toInt), r.toInt)
      .toSet

  def solve1(bots: Set[Bot]): Int =
    val strongest = bots.maxBy(_.radius)
    val reachable = bots.count(_.loc.manhattan(strongest.loc) <= strongest.radius)
    reachable


  def solve2(bots: Set[Bot]): Int =
    def overlapping(bot: Bot): (Bot, Set[Bot]) = bot -> bots.filter(other => bot != other && bot.overlapsWith(other))
    val overlappingRadiiOf: Map[Bot, Set[Bot]] = bots.map(overlapping).toMap
    maximumClique(overlappingRadiiOf).map(bot => bot.loc.manhattan(Location.origin) - bot.radius).max

  override lazy val answer1: Int = solve1(bots)
  override lazy val answer2: Int = solve2(bots)
