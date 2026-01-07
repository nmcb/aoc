package aoc2015

import nmcb.*

object Day09 extends AoC:

  type Node = String
  type Edge = (from: Node, to: Node, distance: Int)

  val edges: Vector[Edge] =
    def parser(s: String): Vector[Edge] =
      s match
        case s"""$f to $t = $d""" => Vector((f, t, d.toInt), (t, f, d.toInt))
        case line: String => sys.error(s"could not parse '$line'")
    lines.flatMap(parser)

  def distance(edges: Vector[Edge], nodes: Vector[Node]): Int =
    assert(nodes.size == 2)
    edges
      .find(e => e.from == nodes(0) && e.to == nodes(1))
      .map(_.distance)
      .getOrElse(sys.error("no route"))

  lazy val answer: Vector[Int] =
    edges
      .map(_.from)
      .distinct
      .permutations
      .map(_.sliding(2).foldLeft(0)((result, route) => result + distance(edges, route)))
      .toVector


  lazy val answer1: Int = answer.min
  lazy val answer2: Int = answer.max
