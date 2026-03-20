package aoc2025

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day04 extends AoC:

  def accessible(rolls: Set[Pos]): Set[Pos] =
    rolls.filter(roll => roll.adjoint8.intersect(rolls).size < 4)

  @tailrec
  def clearAll(rolls: Set[Pos]): Set[Pos] =
    val remove = accessible(rolls)
    if remove.isEmpty then rolls else clearAll(rolls -- remove)


  val rolls: Set[Pos] = Grid.fromLines(lines).filter(_.element == '@').map(_.pos)

  override lazy val answer1: Int = accessible(rolls).size
  override lazy val answer2: Int = (rolls -- clearAll(rolls)).size
