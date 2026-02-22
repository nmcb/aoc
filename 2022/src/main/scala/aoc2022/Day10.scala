package aoc2022

import nmcb.*

import scala.annotation.tailrec

object Day10 extends AoC:

  enum Inst derives CanEqual:
    case Nop
    case Add(value: Int, steps: Int = 2)

  import Inst.*

  lazy val instructions: Vector[Inst] =
    lines
      .map(_.trim)
      .map:
        case s"noop"    => Nop
        case s"addx $v" => Add(v.toInt)

  case class CPU(instructions: Vector[Inst], cycle: Int = 0, x: Int = 1):

    val sync: Vector[Int] =
      Vector(20, 60, 100, 140, 180, 220).map(_ - 1)

    def nextCycle: CPU =
      instructions.runtimeChecked match
        case Nop       +: rest => CPU(            rest, cycle + 1, x    )
        case Add(v, 2) +: rest => CPU(Add(v,1) +: rest, cycle + 1, x    )
        case Add(v, 1) +: rest => CPU(            rest, cycle + 1, x + v)

    val signalStrength: Int =
      x * (cycle + 1)

    val sprite: Vector[Int] =
      Vector(x - 1, x , x + 1)

    val draw: Char =
      if sprite.contains(cycle % 40) then '#' else '.'

  def solve1(cpu: CPU): Int =
    @tailrec
    def loop(current: CPU, acc: Int = 0): Int =
      if current.cycle > current.sync.max then
        acc
      else if current.sync.contains(current.cycle) then
        loop(current.nextCycle, acc + current.signalStrength)
      else
        loop(current.nextCycle, acc)
    loop(cpu)

  def solve2(cpu: CPU): String =
    @tailrec
    def loop(current: CPU, pixels: String = ""): String =
      if current.instructions.isEmpty then
        pixels.grouped(40).mkString("\n")
      else
        loop(current.nextCycle, pixels :+ current.draw)
    loop(cpu)


  val cpu = CPU(instructions)

  override lazy val answer1: Int    = solve1(cpu)
  override lazy val answer2: String = solve2(cpu)
