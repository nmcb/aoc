package aoc2017

import nmcb.*

import scala.annotation.tailrec
import scala.collection.Iterator.single

object Day24 extends AoC:

  case class Component(portA: Int, portB: Int):

    def contains(port: Int): Boolean =
      port == portA || port == portB

    def reverse(port: Int): Int =
      if port == portA then portB else portA

    def strength: Int =
      portA + portB

  type Bridge = Vector[Component]

  extension (bridge: Bridge)
    
    def strength: Int =
      bridge.map(_.strength).sum

  def bridges(components: Set[Component]): Vector[Bridge] =

    case class State(remaining: Set[Component], port: Int, current: Bridge)

    @tailrec
    def loop(stack: List[State], result: Vector[Bridge] = Vector.empty): Vector[Bridge] =
      stack match
        case Nil =>
          result
        case State(remaining, port, current) :: rest =>
          val choices = remaining.filter(_.contains(port)).toList
          val next    = choices.map(component => State(remaining - component, component.reverse(port), current :+ component))
          loop(next ::: rest, result :+ current)


    loop(List(State(components, 0, Vector.empty)))

  val components: Set[Component] =
    lines
      .collect:
        case s"$portA/$portB" => Component(portA.toInt, portB.toInt)
      .toSet

  override lazy val answer1: Long = bridges(components).map(_.strength).max
  override lazy val answer2: Int  = bridges(components).map(bridge => (bridge.length, bridge.strength)).max.last
