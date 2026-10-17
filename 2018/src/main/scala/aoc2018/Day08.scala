package aoc2018

import nmcb.*

import scala.annotation.tailrec

object Day08 extends AoC:

  case class Node(children: Vector[Node], meta: Vector[Int]):

    lazy val nrOfChildren: Int =
      children.size

    lazy val nrOfMeta: Int =
      meta.size

    lazy val licenceNumberPart1: Int =
      meta.sum + children.map(_.licenceNumberPart1).sum

    lazy val licenceNumberPart2: Int =
      if children.isEmpty then
        meta.sum
      else
        meta.map(idx => children.lift(idx - 1).map(_.licenceNumberPart2).getOrElse(0)).sum

  object Node:

    def parseTree(s: String): Node =

      case class ParseState(todoChildren: Int, todoMeta: Int, children: Vector[Node])

      @tailrec
      def parse(input: Vector[Int], stack: List[ParseState]): Node =

        stack.runtimeChecked match

          case ParseState(0, todoMeta, children) :: parentStack =>
            val metadata       = input.take(todoMeta)
            val node           = Node(children, metadata)
            val remainingInput = input.drop(todoMeta)

            parentStack match
              case Nil =>
                node
              case parentState :: grandParentStack =>
                val updatedParent = parentState.copy(
                  todoChildren = parentState.todoChildren - 1,
                  children     = parentState.children :+ node
                )
                parse(remainingInput, updatedParent :: grandParentStack)

          case ParseState(todoChildren, todoMeta, children) :: parentStack =>
            val childChildren = input(0)
            val childMeta     = input(1)
            val nextInput     = input.drop(2)

            val childState = ParseState(childChildren, childMeta, Vector.empty)
            parse(nextInput, childState :: stack)

      val input        = s.trim.split(' ').map(_.toInt).toVector
      val todoChildren = input.head
      val todoMeta     = input.drop(1).head
      val initialInput = input.drop(2)

      val rootState = ParseState(todoChildren, todoMeta, Vector.empty)
      parse(initialInput, List(rootState))

  override lazy val answer1: Int = Node.parseTree(input).licenceNumberPart1
  override lazy val answer2: Int = Node.parseTree(input).licenceNumberPart2
