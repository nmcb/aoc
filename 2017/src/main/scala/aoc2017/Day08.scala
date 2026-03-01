package aoc2017

import nmcb.*
import scala.annotation.tailrec

object Day08 extends AoC:

  type Registers = Map[String, Int]

  case class Inst(target: String, operation: Int => Int, condition: Registers => Boolean)

  case class CPU(instructions: List[Inst], registers: Registers, largestHeld: Int = 0):
    @tailrec
    final def run: CPU =
      instructions match
        case Nil =>
          this
        case inst :: rest =>
          CPU( instructions =
                 rest
             , registers =
                 if inst.condition(registers) then
                   registers.updatedWith(inst.target):
                     case None    => Some(inst.operation(0))
                     case Some(v) => Some(inst.operation(v))
                 else
                   registers
             , largestHeld =
                 if largestValue > largestHeld then
                   largestValue
                 else
                   largestHeld
          ).run

    def largestValue: Int =
      if registers.isEmpty then 0 else registers.valuesIterator.max

    def largestHeldValue: Int =
      largestHeld

  val cpu: CPU =
    val instructions: List[Inst] =
      lines
        .map:
          case s"$a $o $n if $b $c $w" =>
            val operation: Int => Int =
              o match
                case "inc" => r => r + n.toInt
                case "dec" => r => r - n.toInt
            val condition: Registers => Boolean =
              c match
                case "==" => rs => rs(b) == w.toInt
                case "!=" => rs => rs(b) != w.toInt
                case "<=" => rs => rs(b) <= w.toInt
                case ">=" => rs => rs(b) >= w.toInt
                case "<"  => rs => rs(b) <  w.toInt
                case ">"  => rs => rs(b) >  w.toInt
            Inst(a, operation, condition)
        .toList
    CPU(instructions, Map.empty.withDefaultValue(0))

  override lazy val answer1: Int = cpu.run.largestValue
  override lazy val answer2: Int = cpu.run.largestHeld
