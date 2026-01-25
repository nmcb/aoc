package aoc2022

import nmcb.*

import scala.collection.*

object Day16 extends AoC:

  case class Valve(name: String, rate: Int, tunnels: Vector[String], closed: Boolean = true)

  val valves: Vector[Valve] =
    lines.map:
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $tunnels" =>
        Valve(name, rate.toInt, tunnels.trim.split(',').map(_.trim).toVector)
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $tunnel" =>
        Valve(name, rate.toInt, Vector(tunnel))

  val reachability: Map[String,Vector[String]] =
    valves.map(v => v.name -> v.tunnels).toMap

  val pressures: Map[String,Int] =
    valves.map(v => v.name -> v.rate).toMap

  val nonzero: Vector[String] =
    valves.filter(_.rate != 0).map(_.name)

  val distances: Map[(String,String),Int] =
    import Dijkstra.*

    val graph: Graph[String] =
      reachability.foldLeft(Graph.empty[String]):
        case (g, (k, v)) =>
          v.foldLeft(g)((g, l) => g.add(Edge(k, l, 1)))
    val map =
      for
        k1 <- valves.map(_.name)
        k2 <- valves.map(_.name)
        best = Dijkstra.run(k1, graph).distanceTo(k2).getOrElse(sys.error(s"no path from $k1 to $k2"))
      yield (k1,k2) -> best
    map.toMap

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

  def solve2(): Int =
    nonzero.combinations(nonzero.length / 2).foldLeft(0): (max, left) =>
      val max2a = solve1(0, 0, 0, "AA", left, 26)
      val right = nonzero.filterNot(v => left.contains(v))
      val max2b = solve1(0, 0, 0, "AA", right, 26)
      if max2a + max2b > max then max2a + max2b else max


  override lazy val answer1: Int = solve1(0, 0, 0, "AA", nonzero, 30)
  override lazy val answer2: Int = solve2()
