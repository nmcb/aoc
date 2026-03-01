package aoc2017

import nmcb.*

object Day24 extends AoC:

  case class Component(portA: Int, portB: Int):

    def contains(port: Int): Boolean =
      port == portA || port == portB

    def reverse(port: Int): Int =
      if port == portA then portB else portA

    def strength: Int =
      portA + portB

  type Bridge = List[Component]

  extension (bridge: Bridge) def strength: Int =
    bridge.map(_.strength).sum

  def bridges(components: Set[Component]): Iterator[Bridge] =
    def go(components: Set[Component], port: Int): Iterator[Bridge] =
      for
        component <- components.filter(_.contains(port)).iterator
        other      = component.reverse(port)
        bridge    <- Iterator.single(Nil) ++ go(components - component, other)
      yield
        component :: bridge
    go(components, 0)

  val components: Set[Component] =
    lines
      .collect:
        case s"$portA/$portB" => Component(portA.toInt, portB.toInt)
      .toSet

  override lazy val answer1: Long = bridges(components).map(_.strength).max
  override lazy val answer2: Int  = bridges(components).map(bridge => (bridge.length, bridge.strength)).max.last
