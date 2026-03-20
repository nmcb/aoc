package aoc2025

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day04 extends AoC:

  val rolls: Set[Pos] =
    val ps = for y <- lines.indices ; x <- lines.head.indices yield (x = x, y = y)
    ps.filter(p => lines(p.y)(p.x) == '@').toSet

  val minPos: Pos =
    rolls.min

  val maxPos: Pos =
    rolls.max

  def neighbours(p: Pos, minPos: Pos, maxPos: Pos): Set[Pos] =
    Pos.offset8.map(_ + p).filter(p => p >= minPos && p <= maxPos)

  def neighbouring(rolls: Set[Pos], p: Pos, minPos: Pos, maxPos: Pos): Set[Pos] =
    neighbours(p, minPos, maxPos).intersect(rolls)

  def accessible(rolls: Set[Pos], minPos: Pos, maxPos: Pos): Set[Pos] =
    rolls.filter(p => neighbouring(rolls, p, minPos, maxPos).size < 4)

  @tailrec
  def clearAll(rolls: Set[Pos], minPos: Pos, maxPos: Pos): Set[Pos] =
    val remove = accessible(rolls, minPos, maxPos)
    if remove.isEmpty then rolls else clearAll(rolls -- remove, minPos, maxPos)

  override lazy val answer1: Int = accessible(rolls, minPos, maxPos).size
  override lazy val answer2: Int = (rolls -- clearAll(rolls, minPos, maxPos)).size
