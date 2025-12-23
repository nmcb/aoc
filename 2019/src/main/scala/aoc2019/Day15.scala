package aoc2019

import nmcb.*
import scala.collection.*
import scala.collection.immutable.Map

object Day15 extends AoC:

  import cpu.*

  case class Pos(x: Int, y: Int):
    def +(that: Pos): Pos = copy(x = x + that.x, y = y + that.y)

  val neighbours: Vector[(Pos,Int)] =
    Vector(Pos(0, -1) -> 1, Pos(0, 1) -> 2, Pos(-1, 0) -> 3, Pos(1, 0) -> 4)

  def move(pos: Pos, cpu: CPU)(delta: Pos, command: Int): (Pos,CPU,Long) =
    val (next, status) = cpu.withInput(command).outputStates.last
    (pos + delta, next, status)

  def dijkstra(cpu: CPU): (Map[Pos,Int], Option[(Pos,CPU)]) =
    val steps = mutable.Map(Pos(0,0) -> 0)
    val todo  = mutable.PriorityQueue(Pos(0,0) -> cpu)(using Ordering.by((point, _) => steps(point)))
    var target = Option.empty[(Pos,CPU)]

    while todo.nonEmpty do
      val (from, cpu) = todo.dequeue
      neighbours
        .map(move(from, cpu))
        .filter((next,_,_) => !steps.contains(next) || steps(from) + 1 < steps(next))
        .filter((_,_,status) => status > 0)
        .foreach: (next, cpu, status) =>
          steps(next) = steps(from) + 1
          todo.enqueue(next -> cpu)
          if status == 2 then target = Some(next -> cpu)

    (steps.toMap, target)

  def solve1(program: Mem): Int =
    val (first, Some(target, _)) = dijkstra(CPU(program)): @unchecked
    first(target)

  def solve2(program: Mem): Int =
    val (_, Some(_, computer)) = dijkstra(CPU(program)): @unchecked
    val (second, _) = dijkstra(computer)
    second.values.max
  
  val program: Mem = Mem.parse(input)

  lazy val answer1: Int = solve1(program)
  lazy val answer2: Int = solve2(program)
