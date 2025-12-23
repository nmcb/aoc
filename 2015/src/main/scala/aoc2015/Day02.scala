package aoc2015

import nmcb.*

object Day02 extends AoC:

  case class Box(l: Int, h: Int, w: Int):
    def area: Int =
      2*l*w + 2*w*h + 2*h*l

    def slack: Int =
      List(l*w, w*h, h*l).min

    def wrap: Int =
      List(2*l + 2*h, 2*l + 2*w, 2*h + 2*w).min

    def volume: Int =
      l*h*w      

  val boxes: Vector[Box] = lines.collect:
    case s"${l}x${h}x${w}" => Box(l.toInt, h.toInt, w.toInt)

  lazy val answer1: Int = boxes.map(b => b.area + b.slack).sum
  lazy val answer2: Int = boxes.map(b => b.volume + b.wrap).sum
