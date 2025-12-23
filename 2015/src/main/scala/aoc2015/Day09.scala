package aoc2015

import nmcb.*
import scala.annotation.*

object Day09 extends AoC:

  type Node = String

  final case class Edge(from: Node, to: Node, distance: Int)

  final case class Graph(neighbours: Map[Node, List[Edge]] = Map.empty):
    def add(edge: Edge): Graph =
      val reverse: Edge = Edge(edge.to, edge.from, edge.distance)
      Graph(
        neighbours
          .updated(edge.from, neighbours.getOrElse(edge.from, List.empty) :+ edge)
          .updated(reverse.from, neighbours.getOrElse(reverse.from, List.empty) :+ reverse)
      )

  object Graph:
    def empty: Graph =
      Graph(Map.empty)

  object Dijkstra {

    import scala.collection.mutable

    type Dist = (Node, Int)

    val sortByDistance: Ordering[Dist] =
      (d1, d2) => d1._2.compareTo(d2._2)

    def run(graph: Graph, from: Node): Result =

      val edgeTo: mutable.Map[Node, Edge] =
        mutable.Map.empty

      val distTo: mutable.Map[Node, Int] =
        mutable.Map.from(graph.neighbours.map((node,_) => node -> Int.MaxValue))

      // init source distance and add to the queue
      distTo += from -> 0
      val sourceEdge = from -> distTo(from)
      val queue = mutable.PriorityQueue[(Node, Int)](sourceEdge)(using sortByDistance)

      while (queue.nonEmpty)
        val (minDistNode, _) = queue.dequeue()
        val edges = graph.neighbours.getOrElse(minDistNode, List.empty)

        edges.foreach(edge =>

          if distTo(edge.to) > distTo(edge.from) + edge.distance then {
            distTo.update(edge.to, distTo(edge.from) + edge.distance)
            edgeTo.update(edge.to, edge)
            if !queue.exists((node,_) => node == edge.to) then
              queue.enqueue((edge.to, distTo(edge.to)))
          }
        )

      Result(edgeTo.toMap, distTo.toMap)
  }

  case class Result(edgeTo: Map[Node,Edge], distancesTo: Map[Node,Int]):

    def pathTo(node: Node): Either[Node, Seq[Edge]] =

      @tailrec def go(edges: List[Edge], node: Node): List[Edge] =
        edgeTo.get(node) match
          case Some(edge) => go(edge +: edges, edge.from)
          case None => edges

      hasEdge(node).map(b => if (!b) Seq.empty else go(List.empty, node))


    def hasEdge(node: Node): Either[Node, Boolean] =
      distancesTo.get(node).map(_ < Int.MaxValue).toRight(s"node=$node does not exist")

    def distanceTo(node: Node): Either[Node, Int] =
      distancesTo.get(node).toRight(s"node=$node does not exist")

  /** Input */

  val edges: List[Edge] =
    def parser(s: String): List[Edge] =
      s match
        case s"""$from to $to = $distance""" => List(Edge(from, to, distance.toInt), Edge(to, from, distance.toInt))
        case line: String => sys.error(s"could not parse '$line'")

    lines.flatMap(parser).toList

  // FFS you have ALL distances between ALL pairs of cities !!! ie
  //     you DON'T need Dijkstra to solve THIS issue omg, omg, OMG

  def distance(nodes: List[Node]): Int =
    assert(nodes.size == 2)
    edges
      .find(e => e.from == nodes(0) && e.to == nodes(1))
      .map(_.distance)
      .getOrElse(sys.error("no route"))

  lazy val answer: List[Int] =
    edges
      .map(_.from)
      .distinct
      .permutations
      .map(_.sliding(2).foldLeft(0)((acc,route) => acc + distance(route)))
      .toList


  lazy val answer1: Int = answer.min
  lazy val answer2: Int = answer.max
