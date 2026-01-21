package aoc2017

import nmcb.*
import nmcb.predef.*

/** @see Credits - https://github.com/sim642 */
object Day07 extends AoC:

  type Nodes = Map[String, Node]

  extension (nodes: Nodes) def parents: Map[String, String] =
    for
      parent -> node <- nodes
      child          <- node.children
    yield
      child -> parent

  val nodes: Nodes =
    lines
      .map:
        case s"$name ($weight) -> $children" =>
          Node(name, weight.toInt, children.split(", ").toVector)
        case s"$name ($weight)" =>
          Node(name, weight.toInt, Vector.empty)
      .map: node =>
        node.name -> node
      .toMap

  case class Node(name: String, weight: Int, children: Vector[String])

  extension (nodes: Nodes) def root: String =
    nodes
      .keys
      .find: child =>
        !nodes.parents.contains(child)
      .getOrElse(sys.error("no root"))

  def sequence[K,A,B](map: Map[K,Either[A,B]]): Either[A,Map[K,B]] =
    map.foldLeft(Right(Map.empty): Either[A,Map[K,B]]):
      case (acc, key -> either) =>
        for
          map   <- acc
          value <- either
        yield
          map + (key -> value)

  extension (nodes: Nodes) def correctBalanceWeight: Int =
    def eitherDiffOrTotalWeight(node: Node): Either[Int,Int] =
      val childResults: Map[String,Either[Int,Int]] =
        node
          .children
          .map(nodes)
          .map: child =>
            child.name -> eitherDiffOrTotalWeight(child)
          .toMap

      sequence(childResults)
        .flatMap(childTotalWeights =>
          val totalWeightChildren = node.children.groupBy(childTotalWeights)
          if totalWeightChildren.size <= 1 then
            Right(node.weight + childTotalWeights.values.sum)
          else
            val badChild = totalWeightChildren.find(_.right.size == 1).get.right.head
            val badChildWeight = nodes(badChild).weight
            val badChildTotalWeight = childTotalWeights(badChild)
            val goodTotalWeight = totalWeightChildren.find(_.right.size > 1).get.left
            Left(goodTotalWeight - (badChildTotalWeight - badChildWeight)))

    val root = nodes.root
    eitherDiffOrTotalWeight(nodes(root)).swap.getOrElse(sys.error("no bad child"))

  lazy val answer1: String = nodes.root
  lazy val answer2: Int    = nodes.correctBalanceWeight
