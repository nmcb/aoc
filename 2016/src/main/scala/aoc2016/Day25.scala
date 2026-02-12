package aoc2016

import nmcb.*

object Day25 extends AoC:

  import Day23.*

  enum Instruction:
    case CPY(x: Operand, y: Operand)
    case INC(x: Operand)
    case DEC(x: Operand)
    case JNZ(x: Operand, y: Operand)
    case OUT(x: Operand)

  object Instruction:

    def fromString(s: String): Instruction =
      s match
        case s"cpy $x $y" => CPY(x.toOperand, y.toOperand)
        case s"inc $x"    => INC(x.toOperand)
        case s"dec $x"    => DEC(x.toOperand)
        case s"jnz $x $y" => JNZ(x.toOperand, y.toOperand)
        case s"out $x"    => OUT(x.toOperand)

  import Instruction.*

  enum State:
    case Run
    case Halted
    case Out(bit: Int)

  import State.*

  case class CPU(instructions: Vector[Instruction], pc: Int = 0, registers: Registers = Map.empty, state: State = Run):

    def halted: Boolean =
      pc >= instructions.size

    def step: CPU =
      if halted then
        copy(state = Halted)
      else
        instructions(pc) match
          case CPY(x, y)                              => copy(pc = pc + 1, registers = registers.update(y, x), state = Run)
          case INC(x)                                 => copy(pc = pc + 1, registers = registers.update(x, x, _ + 1), state = Run)
          case DEC(x)                                 => copy(pc = pc + 1, registers = registers.update(x, x, _ - 1), state = Run)
          case JNZ(x, y) if registers.valueOf(x) != 0 => copy(pc = pc + registers.valueOf(y), state = Run)
          case JNZ(x, y)                              => copy(pc = pc + 1, state = Run)
          case OUT(x)                                 => copy(pc = pc + 1, state = Out(registers.valueOf(x) & 0x1))

  val instructions: Vector[Instruction] = lines.map(Instruction.fromString)

  def experiment(instructions: Vector[Instruction], size: Int)(a: Int): String =
    Iterator.iterate(CPU(instructions, registers = Map("a" -> a)))(_.step)
      .collect:
        case CPU(_, _, _, Out(bit)) => bit
      .take(size)
      .mkString

  def solve(instructions: Vector[Instruction]): Int =
    val sampleSize = 32
    val sample     = List.tabulate(sampleSize)(n => if n % 2 == 0 then '0' else '1').mkString
    Iterator.from(0).map(experiment(instructions, sampleSize)).indexWhere(_ == sample)

  override lazy val answer1: Int = solve(instructions)
