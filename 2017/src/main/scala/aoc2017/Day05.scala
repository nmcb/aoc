package aoc2017

import nmcb.*
import scala.annotation.tailrec

object Day05 extends AoC:

  val jumps: Vector[Int] = lines.map(_.trim.toInt)

  case class CPU(mem: Vector[Int], update: Int => Int, pc: Int = 0, steps: Int = 0):
    @tailrec
    final def run: CPU =
      mem.lift(pc) match
        case None =>
          this
        case Some(offset) =>
          CPU(mem.updated(pc, update(offset)), update, pc + offset, steps + 1).run

  lazy val answer1: Int = CPU(jumps, offset => offset + 1).run.steps
  lazy val answer2: Int = CPU(jumps, offset => if offset >= 3 then offset - 1 else offset + 1).run.steps
