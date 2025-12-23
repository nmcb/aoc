package aoc2017

import nmcb.*

object Day15 extends AoC:

  def next(factor: Long): Long => Long =
    (current: Long) => current * factor % 2147483647L

  def same(a: Long, b: Long): Boolean =
    (a & 0xffff) == (b & 0xffff)

  def generatorA: Iterator[Long] = Iterator.iterate(591L)(next(16807)).drop(1)
  def generatorB: Iterator[Long] = Iterator.iterate(393L)(next(48271)).drop(1)
  def generator1: Iterator[(Long, Long)] = generatorA zip generatorB
  def generator2: Iterator[(Long, Long)] = generatorA.filter(_ % 4 == 0) zip generatorB.filter(_ % 8 == 0)

  lazy val answer1: Long = generator1.take(40000000).count(same)
  lazy val answer2: Long = generator2.take(5000000).count(same)
