package aoc2016

import nmcb.*
import nmcb.pos.*

import scala.collection.*
import scala.collection.immutable.Map

object Day13 extends AoC:

  val favorite = 1352

  extension (p: Pos)
  
    def isOpen: Boolean =
      val number = p.x*p.x + 3*p.x + 2*p.x*p.y + p.y + p.y*p.y + favorite
      val binary = number.toBinaryString
      binary.count(_ == '1') % 2 == 0

    def candidates: Set[Pos] =
      p.adjoint4.filter(c => c.x >= 0 && c.y >= 0)


  /** breadth first search */
  def solve1(start: Pos, target: Pos): Int =
    val todo    = mutable.Queue(start)
    val cache   = mutable.Map(start -> 0)
    var current = todo.dequeue

    while current != target do
      val steps = cache(current) + 1
      current.candidates.filter(_.isOpen).foreach: next =>
        if !cache.contains(next) || steps < cache(next) then
          cache(next) = steps
          todo.enqueue(next)
      current = todo.dequeue

    cache(target)

  /** breadth first search */
  def solve2(start: Pos): Map[Pos,Int] =
    val todo = mutable.Queue(start)
    val cache = mutable.Map(start -> 0)

    while todo.nonEmpty do
      val current = todo.dequeue
      val cost = cache(current) + 1
      current.candidates.filter(_.isOpen).foreach: next =>
        if (!cache.contains(next) || cost < cache(next)) && cost <= 50 then
          cache(next) = cost
          todo.enqueue(next)

    cache.toMap

  lazy val answer1: Int = solve1(start = Pos.of(1, 1), target = Pos.of(31, 39))
  lazy val answer2: Int = solve2(start = Pos.of(1,1)).size
