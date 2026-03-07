package aoc2015

import nmcb.*

object Day02 extends AoC:

  case class Box(length: Int, height: Int, width: Int):

    def area: Int =
      2 * length * width + 2 * width * height + 2 * height * length

    def slack: Int =
      List(length * width, width * height, height * length).min

    def wrap: Int =
      List(2 * length + 2 * height, 2 * length + 2 * width, 2 * height + 2 * width).min

    def volume: Int =
      length * height * width

  val boxes: Vector[Box] = lines.collect:
    case s"${length}x${height}x${width}" => Box(length.toInt, height.toInt, width.toInt)

  override lazy val answer1: Int = boxes.map(b => b.area + b.slack).sum
  override lazy val answer2: Int = boxes.map(b => b.volume + b.wrap).sum
