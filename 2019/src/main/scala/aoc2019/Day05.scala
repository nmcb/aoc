package aoc2019

import nmcb.*

object Day05 extends AoC:

  import cpu.*

  val program: Mem = Mem.load(input)

  override lazy val answer1: Value = CPU(program).withInput(1).outputs.last
  override lazy val answer2: Value = CPU(program).withInput(5).outputs.last
