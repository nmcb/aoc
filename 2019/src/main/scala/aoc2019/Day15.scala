package aoc2019

import nmcb.*
import nmcb.pos.*

import scala.collection.*
import scala.collection.immutable.Map

object Day15 extends AoC:

  import cpu.*

  type Command   = Int
  type Neighbour = (delta: Pos, command: Command)
  type Tile      = (pos: Pos, cpu: CPU)
  type Output    = (pos: Pos, cpu: CPU, status: Value)
  type Result    = (steps: Map[Pos, Command], target: Option[Tile])

  val neighbours: Vector[(Pos, Command)] =
    Vector(
      ( 0, -1) -> 1,
      ( 0,  1) -> 2,
      (-1,  0) -> 3,
      ( 1,  0) -> 4
    )

  def move(from: Tile, neighbour: Neighbour): Output =
    val (next, status) = from.cpu.withInput(neighbour.command).outputStates.last
    (pos = from.pos + neighbour.delta, cpu = next, status = status)

  /** Functional style Dijkstra with exercise specific filtering and result type */
  def shortestPath(cpu: CPU): Result =
    val steps  = mutable.Map(Pos.origin -> 0)
    val todo   = mutable.PriorityQueue[Tile]((0, 0) -> cpu)(using Ordering.by((point, _) => steps(point)))
    var target = Option.empty[Tile]

    while todo.nonEmpty do
      val from = todo.dequeue()
      neighbours
        .map(n => move(from, n))
        .filter(output => !steps.contains(output.pos) || steps(from.pos) + 1 < steps(output.pos))
        .filter(output => output.status > 0)
        .foreach: output =>
          steps.update(output.pos, steps(from.pos) + 1)
          val next = output.pos -> output.cpu
          todo.enqueue(next)
          if output.status == 2 then target = Some(next)

    (steps = steps.toMap, target = target)

  def solve1(program: Mem): Int =
    val result = shortestPath(CPU(program))
    val steps  = result.steps
    steps(result.target.get.pos)

  def solve2(program: Mem): Int =
    val first  = shortestPath(CPU(program))
    val second = shortestPath(first.target.get.cpu)
    second.steps.values.max
  
  val program: Mem = Mem.load(input)

  override lazy val answer1: Int = solve1(program)
  override lazy val answer2: Int = solve2(program)
