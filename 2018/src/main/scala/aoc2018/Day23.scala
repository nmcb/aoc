package aoc2018

import nmcb.*

object Day23 extends AoC:

  case class Location(x: Int, y: Int, z: Int):
    infix def manhattan(that: Location): Int = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

  object Location:
    val origin: Location = Location(0,0,0)

  case class Bot(loc: Location, radius: Int):
    infix def hasOverlappingRadiusWith(that: Bot): Boolean =
      loc.manhattan(that.loc) <= radius + that.radius

  given CanEqual[Bot, Bot] = CanEqual.derived

  val bots: Set[Bot] =
    lines
      .collect:
        case s"pos=<$x,$y,$z>, r=$r" => Bot(Location(x.toInt, y.toInt, z.toInt), r.toInt)
      .toSet

  def solve1(bots: Set[Bot]): Int =
    val strongest = bots.maxBy(_.radius)
    val reachable = bots.count(_.loc.manhattan(strongest.loc) <= strongest.radius)
    reachable

  /** Computes the maximum clique, i.e. the largest group of bots with overlapping radiuses. */
  def bronKerbosch(overlappingRadiiOf: Map[Bot,Set[Bot]]): Set[Bot] =
    var maximum: Set[Bot] = Set.empty

    def loop(r: Set[Bot], p: Set[Bot], x: Set[Bot]): Unit =
      if p.isEmpty && x.isEmpty then
        if r.size > maximum.size then maximum = r
      else
        // Technically this implements Tomita's optimisation which recurses with a pivot node
        // chosen to contain the maximum number of edges. This variant of the Bron-Kerbosch
        // algorithm performs up to twice as fast on sparse, and similar on dense graphs.
        val u = (p union x).maxBy(overlappingRadiiOf(_).size)
        var bp = p
        var bx = x
        for
          v <- p diff overlappingRadiiOf(u)
        yield
          loop(r + v, p intersect overlappingRadiiOf(v), x intersect overlappingRadiiOf(v))
          bp -= v
          bx += v

    loop(Set.empty, overlappingRadiiOf.keySet, Set.empty)
    maximum

  def solve2(bots: Set[Bot]): Int =
    val overlappingRadiiOf: Map[Bot,Set[Bot]] =
      bots
        .map: bot =>
          bot -> bots.filter: other =>
            bot != other && bot.hasOverlappingRadiusWith(other)
        .toMap

    bronKerbosch(overlappingRadiiOf).map(bot => bot.loc.manhattan(Location.origin) - bot.radius).max

  override lazy val answer1: Int = solve1(bots)
  override lazy val answer2: Int = solve2(bots)
