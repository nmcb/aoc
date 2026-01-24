package aoc2019

import nmcb.*

object Day09 extends AoC:

  import cpu.*

  val program: Mem = Mem.parse(input)

  override lazy val answer1: Value = CPU(program).withInput(1).outputs.last
  override lazy val answer2: Value = CPU(program).withInput(2).outputs.last
