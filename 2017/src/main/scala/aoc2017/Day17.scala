package aoc2017

import nmcb.*
import nmcb.predef.*

import scala.collection.Iterator.iterate

object Day17 extends AoC:

  case class SpinLockPart1(elements: Vector[Int] = Vector(0), current: Int = 0):

    infix def spin(steps: Int): SpinLockPart1 =
      val index         = (current + steps) % elements.size + 1
      val (left, right) = elements.splitAt(index)
      val value         = elements(current) + 1
      copy(elements = left.appended(value).appendedAll(right), current = index)

    def nextValue: Int =
      val index = (current + 1) % elements.size
      elements(index)

  case class SpinLockPart2(size: Int = 1, value: Int = 0, current: Int = 0):

    infix def spin(steps: Int): SpinLockPart2 =
      val index = (current + steps) % size + 1
      copy(size = size + 1, value = if index == 1 then size else value, current = index)

  override lazy val answer1: Int = iterate(SpinLockPart1())(_ spin 370).nth(2017).nextValue
  override lazy val answer2: Int = iterate(SpinLockPart2())(_ spin 370).nth(50000000).value
