package aoc2024

import nmcb.*
import nmcb.predef.*

/** @see Credits - https://github.com/sim642 */
object Day23 extends AoC:

  type Node = String

  lazy val edges: Set[(Node, Node)] =

    val directed = lines.toSet.map:
      case s"$a-$b" => (a, b)

    directed ++ directed.map(_.swap)

  lazy val neighbours: Map[Node, Set[Node]] =
    edges.groupMap(_.left)(_.right)

  lazy val solve1: Set[Set[Node]] =
    for
      (a, b) <- edges
      c      <- neighbours(a) intersect neighbours(b)
      if a.startsWith("t") || b.startsWith("t") || c.startsWith("t")
    yield
      Set(a, b, c)

  override lazy val answer1: Long = solve1.size
  override lazy val answer2: String = BronKerbosch.run(neighbours).toVector.sorted.mkString(",")
