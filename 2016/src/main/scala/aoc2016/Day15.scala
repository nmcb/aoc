package aoc2016

import nmcb.*
import nmcb.predef.*

object Day15 extends AoC:

  case class Disc(nr: Int, positions: Int, position: Int):
    def passWhenDroppedAt(time: Int): Boolean =
      (nr + position + time) % positions == 0

  type Discs = Vector[Disc]

  lazy val discs: Discs = lines.map:
    case s"Disc #$nr has $positions positions; at time=0, it is at position $position." =>
      Disc(nr.toInt, positions.toInt, position.toInt)

  def validDrop(discs: Discs)(time: Int): Boolean =
    discs.forall(_.passWhenDroppedAt(time))

  lazy val answer1: Int = Iterator.from(0).findFirst(validDrop(discs))
  lazy val answer2: Int = Iterator.from(0).findFirst(validDrop(discs :+ Disc(nr = 7, positions = 11, position = 0)))
