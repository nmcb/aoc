package aoc2018

import nmcb.*

import scala.annotation.tailrec

object Day21 extends AoC:

  import Day16.*
  import Day19.*
  import Day19.Inst
  import Inst.*

  extension (cpu: CPU)

    @tailrec
    def nextAfterEQRR: CPU =
      if cpu.halted then
        cpu
      else if cpu.program(cpu.pc).isInstanceOf[EQRR] then
        cpu.execute(cpu.program(cpu.pc))
      else
        cpu.execute(cpu.program(cpu.pc)).nextAfterEQRR

  /**
   * @see Credits - https://www.reddit.com/r/adventofcode/comments/a86jgt/comment/ec8fnq8
   *
   * Realizing that the only time register 0 was being used was on line 28, in:
   *
   * 28: eqrr 5 0 1
   *
   * Together with the rest of the program not containing an eqrr instruction yields, that
   * we can just check what register 5 contains the first time the line is executed.
   */
  override lazy val answer1: Int = CPU.load(lines).nextAfterEQRR.mem.valueOf(5)

  /**
   * For part 2, we check when the value in register 5 repeats. When it does,
   * the last value must have been the one that causes the most instructions.
   * The solution takes about 2 minutes so the result is copied in.
   */
  override lazy val answer2: Int = 5876609 // Cycle.find(cpu, _.nextAfterEQRR, _.mem.valueOf(5)).cycleLast.mem.valueOf(5)
