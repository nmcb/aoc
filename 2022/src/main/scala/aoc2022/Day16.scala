package aoc2022

import nmcb.*

object Day16 extends AoC:

  case class Valve(name: String, rate: Int, tunnels: Vector[String], closed: Boolean = true)

  val valves: Vector[Valve] =
    lines.collect:
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $tunnels" =>
        Valve(name, rate.toInt, tunnels.trim.split(',').map(_.trim).toVector)
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $tunnel" =>
        Valve(name, rate.toInt, Vector(tunnel))

  val reachability: Map[String, Set[String]] =
    valves.map(v => v.name -> v.tunnels.toSet).toMap

  val pressures: Map[String, Int] =
    valves.map(v => v.name -> v.rate).toMap

  val nonzero: Vector[String] =
    valves.filter(_.rate != 0).map(_.name)

  val distances: Map[(String, String), Int] =
    val result =
      for
        valve1 <- valves.map(_.name)
        valve2 <- valves.map(_.name)
        if valve1 != valve2
      yield
        (valve1, valve2) -> Dijkstra
          .runWithUnitDistances(valve1, reachability)
          .distanceTo(valve2)
          .getOrElse(sys.error(s"no path from $valve1 to $valve2"))

    result.toMap

  def solve1(time: Int, pressure: Int, flow: Int, current: String, remaining: Vector[String], limit: Int): Int =
    remaining.foldLeft(pressure + (limit - time) * flow): (max, v) =>
      val steps = distances((current, v)) + 1
      if (time + steps) < limit then
        val t = time + steps
        val p = pressure + (steps * flow)
        val f = flow + pressures(v)
        val score = solve1(t, p, f, v, remaining.filter(_ != v), limit)
        if score > max then
          score
        else
          max
      else
        max

  def solve2(time: Int, pressure: Int, flow: Int, current: String, nonzero: Vector[String], limit: Int): Int =
    nonzero.combinations(nonzero.length / 2).foldLeft(0): (max, left) =>
      val max2a = solve1(time, pressure, flow, current, left, limit)
      val right = nonzero.filterNot(v => left.contains(v))
      val max2b = solve1(time, pressure, flow, current, right, limit)
      if max2a + max2b > max then max2a + max2b else max


  override lazy val answer1: Int = solve1(0, 0, 0, "AA", nonzero, 30)
  override lazy val answer2: Int = solve2(0, 0, 0, "AA", nonzero, 26)
