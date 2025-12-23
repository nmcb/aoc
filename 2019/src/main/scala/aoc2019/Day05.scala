package aoc2019

import nmcb.*

object Day05 extends AoC:

  import cpu.*

  val program: Mem = Mem.parse(input)

  lazy val answer1: Value = CPU(mem = program, stdin = LazyList(1)).outputs.last
  lazy val answer2: Value = CPU(mem = program, stdin = LazyList(5)).outputs.last
