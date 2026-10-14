package aoc2017

import nmcb.*
import nmcb.predef.*

import scala.collection.Iterator.iterate

object Day17 extends AoC:

  case class RingBuffer(elements: Vector[Int], current: Int):

    infix def insert(steps: Int): RingBuffer =
      val value             = elements(current) + 1
      val index             = (current + steps) % elements.size + 1
      val (prefix, postfix) = elements.splitAt(index)
      val nextElements      = prefix.appended(value).appendedAll(postfix)
      copy(elements = nextElements, current = index)

    def nextValue: Int =
      val index = (current + 1) % elements.size
      elements(index)

  object RingBuffer:

    def init: RingBuffer =
      RingBuffer(elements = Vector(0), current = 0)

    def spin(steps: Int, amount: Int): RingBuffer =
      iterate(RingBuffer.init)(_ insert steps).nth(amount)

  case class SpinLock(size: Int, value: Int, current: Int):

    infix def insert(steps: Int): SpinLock =
      val index = (current + steps) % size + 1
      copy(
        size    = size + 1,
        value   = if index == 1 then size else value,
        current = index
      )

  object SpinLock:

    def init: SpinLock =
      SpinLock(size = 1, value = 0, current = 0)

    def spin(steps: Int, amount: Int): SpinLock =
      iterate(SpinLock.init)(_ insert steps).nth(amount)


  override lazy val answer1: Int = RingBuffer.spin(370, 2017).nextValue
  override lazy val answer2: Int = SpinLock.spin(370, 50000000).value
