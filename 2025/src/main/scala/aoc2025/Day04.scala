package aoc2025

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day04 extends AoC:

  val (minPos, maxPos, rolls) =
    val ps = for y <- lines.indices ; x <- lines.head.indices yield Pos.of(x, y)
    (ps.min, ps.max, ps.filter(p => lines(p.y)(p.x) == '@').toSet)

  extension (p: Pos)

    def neighbours: Set[Pos] =
      Pos.offset8.map(_ + p).filter(p => p >= minPos && p <= maxPos)

  extension (rolls: Set[Pos])

    def neighbouring(p: Pos): Set[Pos] =
      p.neighbours.intersect(rolls)

    def accessible: Set[Pos] =
      rolls.filter(p => rolls.neighbouring(p).size < 4)

    @tailrec
    def clearAll: Set[Pos] =
      val remove = accessible
      if remove.isEmpty then rolls else (rolls -- remove).clearAll

  override lazy val answer1: Int = rolls.accessible.size
  override lazy val answer2: Int = (rolls -- rolls.clearAll).size
