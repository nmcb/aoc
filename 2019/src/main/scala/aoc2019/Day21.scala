package aoc2019

import nmcb.*

object Day21 extends AoC:

  import cpu.*

  /** ((A | -(B & C)) & D) */
  val walk =
    Seq(
      "OR A J",
      "AND B J",
      "AND C J",
      "NOT J J",
      "AND D J"
    )

  /** (-A | -C | -B) & D & (E | H) */
  val run =
    Seq(
      "NOT A J",
      "NOT C T",
      "OR T J",
      "NOT B T",
      "OR T J",
      "AND D J",
      "NOT D T",
      "OR E T",
      "OR H T",
      "AND T J"
    )

  def survey(program: Mem, script: Seq[String]): Long =
    val input = script.map(_ + "\n").mkString.map(_.toLong)
    CPU(program).withInput(input *).outputs.last

  val program: Mem = Mem.parse(input)

  lazy val answer1: Long = survey(program, script = walk :+ "WALK")
  lazy val answer2: Long = survey(program, script = run :+ "RUN")
