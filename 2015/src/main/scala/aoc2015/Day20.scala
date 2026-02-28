package aoc2015

import nmcb.*
import nmcb.predef.*

object Day20 extends AoC:

  def solve1(atLeastPresents: Int): Int =
    val elfCount = atLeastPresents / 10
    val presentCount: Array[Int] = Array.fill(elfCount + 1)(0)
    for
      elf   <- Range.inclusive(1, elfCount)
      house <- Range.inclusive(elf, elfCount, elf)
    do
      presentCount(house) = presentCount(house) + elf * 10
    presentCount.zipWithIndex.find(count => count.element >= atLeastPresents).get.index

  def solve2(atLeastPresents: Int): Int =
    val elfCount = atLeastPresents / 10 + 1
    val presentCount: Array[Int] = Array.fill(elfCount)(0)
    for
      elf   <- Range.inclusive(1, elfCount)
      house <- Range.inclusive(elf, elf * 50, elf)
      if house < elfCount
    yield
      presentCount(house) = presentCount(house) + elf * 11
    presentCount.zipWithIndex.find(count => count.element >= atLeastPresents).get.index

  override lazy val answer1: Int = solve1(atLeastPresents = 36_000_000)
  override lazy val answer2: Int = solve2(atLeastPresents = 36_000_000)
