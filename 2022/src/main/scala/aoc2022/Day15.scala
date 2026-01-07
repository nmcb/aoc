package aoc2022

import nmcb.*
import nmcb.pos.*

import scala.io.*

object Day15 extends AoC:

  private case class Cover(min: Long, max: Long)

  private case class Lock(sensor: Pos, beacon: Pos):

    def cover(y: Int): Option[Cover] =
      if y >= sensor.y - sensor.manhattan(beacon) && y <= sensor.y + sensor.manhattan(beacon) then
        val min = sensor.x + math.abs(y - sensor.y) - sensor.manhattan(beacon)
        val max = sensor.x - math.abs(y - sensor.y) + sensor.manhattan(beacon)
        Some(Cover(min, max))
      else
        None

  private val locks: List[Lock] =
    Source
      .fromResource(s"$day.txt")
      .getLines()
      .toList
      .map:
        case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
          Lock((x = sx.toInt, y = sy.toInt), (x = bx.toInt, y = by.toInt))

  lazy val answer1: Int =
    val covers: List[Cover] = locks.flatMap(_.cover(2000000))
    val maxX: Long = math.max(covers.map(_.max).max, 2000000)
    val minX: Long = math.min(covers.map(_.min).min, 0)
    (minX to maxX).foldLeft(0): (count, x) =>
      if covers.exists(c => c.min <= x && c.max >= x) then count + 1 else count

  lazy val answer2: Long =
    var answer: Long = 0
    for
      y <- 0 to 4000000
    yield
      val lcs = locks.flatMap(_.cover(y)).sortBy(_.min)
      lcs.foldLeft(0L): (x, cur) =>
          if cur.min > x then answer = x * 4000000 + y
          math.max(x + 1, cur.max)
    answer
