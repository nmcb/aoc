package aoc2015

import nmcb.*

object Day09 extends AoC:

  type Node = String
  type Edge = (source: Node, target: Node, distance: Int)

  val edges: Vector[Edge] =
    lines.flatMap:
      case s"""$source to $target = $distance""" =>
        Vector((source, target, distance.toInt), (target, source, distance.toInt))

  def distance(edges: Vector[Edge], source: Node, target: Node): Int =
    edges
      .find(e => e.source == source && e.target == target)
      .map(_.distance)
      .getOrElse(sys.error(s"no route from $source to $target"))

  def solve(edges: Vector[Edge]): Vector[Int] =
    edges
      .map(_.source)
      .distinct
      .permutations
      .map(_.sliding(2).foldLeft(0)((result, route) => result + distance(edges, route(0), route(1))))
      .toVector


  override lazy val answer1: Int = solve(edges).min
  override lazy val answer2: Int = solve(edges).max
