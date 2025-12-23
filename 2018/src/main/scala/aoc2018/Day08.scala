package aoc2018

import nmcb.*

object Day08 extends AoC:

  case class Node(children: List[Node], meta: List[Int]):
    def nrOfChilds: Int     = children.size
    def nrOfMeta: Int       = meta.size
    def licenceNumber: Int  = meta.sum + children.map(_.licenceNumber).sum
    def value: Int          = if children.isEmpty then meta.sum else meta.map(idx => children.lift(idx - 1).map(_.value).getOrElse(0)).sum

  object Node:

    def parseTree(s: String): Node =
      def loop(input: List[Int], todoChildren: Int, todoMeta: Int, children: List[Node] = List.empty): (Node, List[Int]) =
        if todoChildren == 0 then
          (Node(children, input.take(todoMeta)), input.drop(todoMeta))
        else
          // parse children
          val todoChildChildren = input.head
          val todoChildMeta = input.drop(1).head
          val (child, rest) = loop(input.drop(2), todoChildChildren, todoChildMeta, List.empty)
          loop(rest, todoChildren - 1, todoMeta, children :+ child)

      val input = s.trim.split(' ').map(_.toInt).toList
      val todoChildren = input.head
      val todoMeta = input.drop(1).head
      val (root, rest) = loop(input.drop(2), todoChildren, todoMeta)
      assert(rest.isEmpty)
      root

  lazy val answer1: Int = Node.parseTree(input).licenceNumber
  lazy val answer2: Int = Node.parseTree(input).value
