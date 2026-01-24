package aoc2019

import nmcb.*

object Day02 extends AoC:

  import cpu.*

  def experiment(program: Mem): Long =
    val permutations =
      for
        noun <- 1 to 99
        verb <- 1 to 99
      yield
        (noun, verb)

    val (noun, verb) =
      permutations
        .dropWhile: (noun, verb) =>
          val patch = program.set(1, noun).set(2, verb)
          CPU(patch).execFinal.mem(0) != 19690720
        .head

    100 * noun + verb

  val program: Mem = Mem.parse(input)

  override lazy val answer1: Value = CPU(program.set(1, 12).set(2, 2)).execFinal.mem(0)
  override lazy val answer2: Value = experiment(program)
