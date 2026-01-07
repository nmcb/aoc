package aoc2015

import nmcb.*

object Day09 extends AoC:

  type Node = String
  type Edge = (from: Node, to: Node, distance: Int)

  val edges: Vector[Edge] =
    lines
      .flatMap:
        case s"""$f to $t = $d""" => Vector((f, t, d.toInt), (t, f, d.toInt))

  def distance(edges: Vector[Edge], from: Node, to: Node): Int =
    edges
      .find(e => e.from == from && e.to == to)
      .map(_.distance)
      .getOrElse(sys.error(s"no route from $from to $to"))

  def solve(edges: Vector[Edge]): Vector[Int] =
    edges
      .map(_.from)
      .distinct
      .permutations
      .map(_.sliding(2).foldLeft(0)((result, route) => result + distance(edges, route(0), route(1))))
      .toVector


  lazy val answer1: Int = solve(edges).min
  lazy val answer2: Int = solve(edges).max
