package aoc2018

import nmcb.*
import scala.annotation.tailrec

object Day19 extends AoC:

  import Day16.*

  enum Inst:
    case ADDR(a: Int, b: Int, c: Int)
    case ADDI(a: Int, b: Int, c: Int)
    case MULR(a: Int, b: Int, c: Int)
    case MULI(a: Int, b: Int, c: Int)
    case BANR(a: Int, b: Int, c: Int)
    case BANI(a: Int, b: Int, c: Int)
    case BORR(a: Int, b: Int, c: Int)
    case BORI(a: Int, b: Int, c: Int)
    case SETR(a: Int, c: Int)
    case SETI(a: Int, c: Int)
    case GTIR(a: Int, b: Int, c: Int)
    case GTRI(a: Int, b: Int, c: Int)
    case GTRR(a: Int, b: Int, c: Int)
    case EQIR(a: Int, b: Int, c: Int)
    case EQRI(a: Int, b: Int, c: Int)
    case EQRR(a: Int, b: Int, c: Int)

    def execute(mem: Mem): Mem =
      this match
        case ADDR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a + b)
        case ADDI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a + b)
        case MULR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a * b)
        case MULI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a * b)
        case BANR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a & b)
        case BANI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a & b)
        case BORR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a | b)
        case BORI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a | b)
        case SETR(a, c)    => mem.setRN(a, c)
        case SETI(a, c)    => mem.setIN(a, c)
        case GTIR(a, b, c) => mem.setIR(a, b, c, (a: Int) => (b: Int) => if a  > b then 1 else 0)
        case GTRI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => if a  > b then 1 else 0)
        case GTRR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => if a  > b then 1 else 0)
        case EQIR(a, b, c) => mem.setIR(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)
        case EQRI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)
        case EQRR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)

  case class CPU(pragma: Int, program: Vector[Inst], pc: Int = 0, mem: Mem = Map.empty):

    def halted: Boolean = !program.indices.contains(pc)

    def execute(inst: Inst): CPU =
      val bind   = this.copy(mem = this.mem.setIN(this.pc, this.pragma))
      val exec   = bind.copy(mem = inst.execute(bind.mem))
      val result = exec.copy(pc  = exec.mem.valueOf(exec.pragma) + 1)
      result

    @tailrec
    final def run: CPU =
      if halted then
        this
      else
        execute(program(pc)).run

  object CPU:

    import Inst.*

    def load(lines: Vector[String]): CPU =
      val pragma = lines.collect:
        case s"#ip $r" => r.toInt

      val program = lines.collect:
        case s"$inst $a $b $c" =>
          inst.toUpperCase match
            case "ADDR" => ADDR(a.toInt, b.toInt, c.toInt)
            case "ADDI" => ADDI(a.toInt, b.toInt, c.toInt)
            case "MULR" => MULR(a.toInt, b.toInt, c.toInt)
            case "MULI" => MULI(a.toInt, b.toInt, c.toInt)
            case "BANR" => BANR(a.toInt, b.toInt, c.toInt)
            case "BANI" => BANI(a.toInt, b.toInt, c.toInt)
            case "BORR" => BORR(a.toInt, b.toInt, c.toInt)
            case "BORI" => BORI(a.toInt, b.toInt, c.toInt)
            case "SETR" => SETR(a.toInt,          c.toInt)
            case "SETI" => SETI(a.toInt,          c.toInt)
            case "GTIR" => GTIR(a.toInt, b.toInt, c.toInt)
            case "GTRI" => GTRI(a.toInt, b.toInt, c.toInt)
            case "GTRR" => GTRR(a.toInt, b.toInt, c.toInt)
            case "EQIR" => EQIR(a.toInt, b.toInt, c.toInt)
            case "EQRI" => EQRI(a.toInt, b.toInt, c.toInt)
            case "EQRR" => EQRR(a.toInt, b.toInt, c.toInt)

      CPU(pragma.head, program)

  /**
   * @see Credits - https://www.reddit.com/r/adventofcode/comments/a7j9zc/comment/ec45g4d/
   *
   * lines = open("day19.txt", "r").readlines()
   * a, b  = int(lines[22].split()[2]), int(lines[24].split()[2])
   * n1 = 836 + 22 * a + b
   * n2 = n1 + 10550400
   *
   */

  val a      = lines(22).split(" ")(2).toInt
  val b      = lines(24).split(" ")(2).toInt
  val nPart1 = 836 + 22 * a + b
  val nPart2 = nPart1 + 10550400

  def sumOfFactorsOf(n: Int): Int =
    @tailrec
    def go(x: Int = 1, total: Int = 0): Int =
      if n % x == 0 then if n / x < x then total else go(x + 1, total + x + n / x) else go(x + 1, total)
    go()

  override lazy val answer1: Int = CPU.load(lines).run.mem.valueOf(0)
  override lazy val answer2: Int = sumOfFactorsOf(nPart2)
