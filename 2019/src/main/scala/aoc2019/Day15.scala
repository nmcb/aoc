package aoc2019

import nmcb.*
import nmcb.pos.*

import scala.collection.*
import scala.collection.immutable.Map

object Day15 extends AoC:

  import cpu.*

  type Command   = Int
  type Movement  = (delta: Pos, command: Command)
  type State     = (pos: Pos, cpu: CPU)
  type Output    = (state: State, status: Value)
  type Result    = (steps: Map[Pos, Command], target: Option[State])

  extension (state: State)
    def move(movement: Movement): Output =
      val (next, status) = state.cpu.withInput(movement.command).outputStates.last
      (state = (pos = state.pos + movement.delta, cpu = next), status = status)

  /** Functional style Dijkstra with exercise specific filtering and result type */
  def shortestPath(cpu: CPU): Result =

    val movements: Vector[Movement] =
      Vector(
        ( 0, -1) -> 1,
        ( 0,  1) -> 2,
        (-1,  0) -> 3,
        ( 1,  0) -> 4
      )

    val steps             = mutable.Map(Pos.origin -> 0)
    given Ordering[State] = Ordering.by(state => steps(state.pos))
    val todo              = mutable.PriorityQueue[State]((0, 0) -> cpu)
    var target            = Option.empty[State]

    while todo.nonEmpty do
      val from = todo.dequeue()
      movements
        .map(adjoint => from.move(adjoint))
        .filter(output => !steps.contains(output.state.pos) || steps(from.pos) + 1 < steps(output.state.pos))
        .filter(output => output.status > 0)
        .foreach: output =>
          steps.update(output.state.pos, steps(from.pos) + 1)
          todo.enqueue(output.state)
          if output.status == 2 then target = Some(output.state)

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
