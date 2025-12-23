package aoc2017

import nmcb.*

object Day13 extends AoC:

  type Layers = Map[Int,Int]

  val layers: Layers =
    lines
      .map:
        case s"$depth: $range" => depth.toInt -> range.toInt
      .toMap

  extension (layers: Layers)

    def rangesCaught(delay: Int): Set[Int] =
      def caught(range: Int, time: Int): Boolean = time % (2 * (range - 1)) == 0
      layers.filter((depth, range) => caught(range, delay + depth)).keySet

    def tripSeverity: Int =
      rangesCaught(delay = 0).map(depth => depth * layers(depth)).sum

    def uncaughtDelay: Int =
      Iterator.from(0).find(delay => layers.rangesCaught(delay).isEmpty).get


  lazy val answer1: Int = layers.tripSeverity
  lazy val answer2: Int = layers.uncaughtDelay
