package aoc2022

import nmcb.*
import nmcb.pos.*

object Day15 extends AoC:

  private case class Cover(min: Long, max: Long)

  private case class Lock(sensor: Pos, beacon: Pos):

    def coverOption(y: Int): Option[Cover] =
      val distance = sensor.manhattanDistance(beacon)
      Option.when(y >= sensor.y - distance && y <= sensor.y + distance):
        val min = sensor.x + math.abs(y - sensor.y) - distance
        val max = sensor.x - math.abs(y - sensor.y) + distance
        Cover(min, max)

  private val locks: Vector[Lock] =
    lines.map:
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        Lock((x = sx.toInt, y = sy.toInt), (x = bx.toInt, y = by.toInt))

  override lazy val answer1: Int =
    val covers: Vector[Cover] = locks.flatMap(_.coverOption(2000000))
    val maxX: Long = covers.map(_.max).max max 2000000
    val minX: Long = covers.map(_.min).min min 0
    (minX to maxX).foldLeft(0): (count, x) =>
      if covers.exists(c => c.min <= x && c.max >= x) then count + 1 else count

  override lazy val answer2: Long =
    var result: Long = 0
    for
      y <- 0 to 4000000
    do
      locks
        .flatMap(_.coverOption(y))
        .sortBy(_.min)
        .foldLeft(0L): (x, cover) =>
          if cover.min > x then result = x * 4000000 + y
          math.max(x + 1, cover.max)
    result
