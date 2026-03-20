package aoc2025

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day04 extends AoC:

  val positions: Seq[Pos] =
    for
      y <- lines.indices
      x <- lines.head.indices
    yield
      (x = x, y = y)

  def accessible(rolls: Set[Pos], minPos: Pos, maxPos: Pos): Set[Pos] =
    def neighbouring(p: Pos): Set[Pos] = p.adjoint8.withinBounds(minPos, maxPos).intersect(rolls)
    rolls.filter(p => neighbouring(p).size < 4)

  @tailrec
  def clearAll(rolls: Set[Pos], minPos: Pos, maxPos: Pos): Set[Pos] =
    val remove = accessible(rolls, minPos, maxPos)
    if remove.isEmpty then rolls else clearAll(rolls -- remove, minPos, maxPos)


  val minPos: Pos = positions.min
  val maxPos: Pos = positions.max
  val rolls: Set[Pos] = positions.filter(p => lines(p.y)(p.x) == '@').toSet

  override lazy val answer1: Int = accessible(rolls, minPos, maxPos).size
  override lazy val answer2: Int = (rolls -- clearAll(rolls, minPos, maxPos)).size
