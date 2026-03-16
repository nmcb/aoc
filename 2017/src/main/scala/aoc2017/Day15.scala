package aoc2017

import nmcb.*
import nmcb.predef.*

object Day15 extends AoC:

  extension (factor: Long)
    def rng: Long => Long =
    (current: Long) => current * factor % 2147483647L

  extension (tuple: (Long, Long))
    def equal32: Boolean =
      (tuple.left & 0xffff) == (tuple.right & 0xffff)

  def generatorA: Iterator[Long] = Iterator.iterate(591L)(16807.rng).drop(1)
  def generatorB: Iterator[Long] = Iterator.iterate(393L)(48271.rng).drop(1)
  def generator1: Iterator[(Long, Long)] = generatorA zip generatorB
  def generator2: Iterator[(Long, Long)] = generatorA.filter(_ % 4 == 0) zip generatorB.filter(_ % 8 == 0)

  override lazy val answer1: Long = generator1.take(40000000).count(_.equal32)
  override lazy val answer2: Long = generator2.take(5000000).count(_.equal32)
