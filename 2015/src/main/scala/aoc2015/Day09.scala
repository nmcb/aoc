package aoc2015

import nmcb.*
import nmcb.predef.*

object Day09 extends AoC:

  type Node = String
  type Edge = (source: Node, target: Node, distance: Int)

  val edges: Vector[Edge] =
    lines.flatMap:
      case s"""$source to $target = $distance""" =>
        Vector((source, target, distance.toInt), (target, source, distance.toInt))

  def distance(edges: Vector[Edge], source: Node, target: Node): Int =
    edges.findFirst(e => e.source == source && e.target == target).distance

  def solve(edges: Vector[Edge]): Vector[Int] =
    edges
      .map(_.source)
      .distinct
      .permutations
      .map(_.sliding(2).foldLeft(0)((result, route) => result + distance(edges, route(0), route(1))))
      .toVector


  override lazy val answer1: Int = solve(edges).min
  override lazy val answer2: Int = solve(edges).max
