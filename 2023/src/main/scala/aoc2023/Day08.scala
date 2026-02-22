package aoc2023

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day08 extends AoC:

  case class Dirs(cycle: String):
    
    def next: (Char, Dirs) =
      (cycle.head, Dirs(cycle.tail + cycle.head))

  type Nodes = Map[String, (String, String)]

  case class Network(directions: Dirs, nodes: Nodes):
    def step(from: String, direction: Char): String =
      direction.runtimeChecked match
        case 'L' => nodes.getOrElse(from, sys.error(s"no step left from: $from")).left
        case 'R' => nodes.getOrElse(from, sys.error(s"no step right from: $from")).right

    @tailrec
    final def pathTo(exit: String => Boolean, from: String, directions: Dirs, path: String = ""): String =
      if exit(from) then
        path
      else
        val (direction, next) = directions.next
        val node = step(from, direction)
        pathTo(exit, node, next, path + direction)

    def steps1: Int =
      pathTo(_ == "ZZZ", "AAA", directions).length

    def gcd(a: Long, b: Long): Long =
      if (b == 0) a.abs else gcd(b, a % b)

    def lcm(a: Long, b: Long): Long =
      (a * b).abs / gcd(a, b)

    def steps2: Long =
      // note: each start node has a repeating path, ending with a node that ends with a 'Z'
      val starts = nodes.keys.filter(_.endsWith("A")).toSet
      val paths  = starts.map(from => pathTo(_.endsWith("Z"), from, directions))
      paths.map(_.length.toLong).fold(1L)(lcm)


  lazy val network: Network =
    
    val directions: Dirs =
      Dirs(lines.head)
    
    val nodes: Nodes =
      lines
        .collect:
          case s"$src = ($left, $right)" => src -> (left, right)
        .toMap
    
    Network(directions, nodes)

  override lazy val answer1: Int  = network.steps1
  override lazy val answer2: Long = network.steps2
