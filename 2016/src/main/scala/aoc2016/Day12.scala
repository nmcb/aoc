package aoc2016

import nmcb.*
import scala.annotation.tailrec
import scala.util.Try

object Day12 extends AoC:

  type Value     = Int
  type Register  = String
  type Operand   = Value | Register
  type Registers = Map[Register, Value]

  extension (registers: Registers)

    def valueOf(operand: Operand): Value =
      operand match
        case r: Register => registers.getOrElse(r, 0)
        case i: Int      => i

    def update(register: Register, operand: Operand, f: Value => Value = identity): Registers =
      val value = (f compose registers.valueOf)(operand)
      registers.updated(register, value)


  enum Instruction:
    case CPY(operand: Operand, register: Register)
    case INC(register: Register)
    case DEC(register: Register)
    case JNZ(operand: Operand, offset: Value)

  object Instruction:

    extension (s: String)
      def toValue: Value       = s.toInt
      def toRegister: Register = s
      def toOperand: Operand   = Try(s.toValue).getOrElse(s)


    def fromString(s: String): Instruction =
      s match
        case s"cpy $x $y" => CPY(x.toOperand, y.toRegister)
        case s"inc $x"    => INC(x.toRegister)
        case s"dec $x"    => DEC(x.toRegister)
        case s"jnz $x $y" => JNZ(x.toOperand, y.toValue)


  case class CPU(instructions: Vector[Instruction], pc: Int = 0, registers: Registers = Map.empty):

    import Instruction.*

    def halted: Boolean =
      pc >= instructions.size

    @tailrec
    final def run: CPU =
      if halted then
        this
      else
        instructions(pc) match
          case CPY(o, r) => copy(pc = pc + 1, registers = registers.update(r, o)).run
          case INC(r)    => copy(pc = pc + 1, registers = registers.update(r, r, _ + 1)).run
          case DEC(r)    => copy(pc = pc + 1, registers = registers.update(r, r, _ - 1)).run
          case JNZ(o, v) => if registers.valueOf(o) != 0 then copy(pc = pc + v).run else copy(pc = pc + 1).run

  lazy val instructions: Vector[Instruction] = lines.map(Instruction.fromString)

  override lazy val answer1: Value = CPU(instructions).run.registers.valueOf("a")
  override lazy val answer2: Value = CPU(instructions = instructions, registers = Map("c" -> 1)).run.registers.valueOf("a")
