package aoc2024

import nmcb.*
import nmcb.pos.{*, given}
import nmcb.predef.*

object Day16 extends AoC:

  import Dir.*
  import Option.*
  import Iterator.*

  type Costs = Map[(Pos, Dir), Long]

  case class Tracer(cost: Long, pos: Pos, dir: Dir, path: Set[Pos]):
    def options: Vector[Tracer] =
      Vector(
        Tracer(cost + 1, pos.step(dir), dir, path + pos.step(dir)),
        Tracer(cost + 1000, pos, dir.cw, path),
        Tracer(cost + 1000, pos, dir.ccw, path)
      )

  case class Maze(grid: Grid[Char]):
    lazy val start: Pos       = grid.findOne('S')
    lazy val end: Pos         = grid.findOne('E')
    lazy val walls: Set[Pos]  = grid.findAll('#')
    lazy val inside: Set[Pos] = grid.findAll('.')

    def advance(tracer: Tracer, costs: Costs): Boolean =
      val inside = !walls.contains(tracer.pos)
      val better = costs.get(tracer.pos -> tracer.dir).forall(_ >= tracer.cost)
      inside & better


  def solve1(maze: Maze): Long =

    case class State(tracers: Vector[Tracer], costs: Costs = Map.empty):

      def next: State =
        tracers.foldLeft(copy(tracers = Vector.empty)): (state, tracer) =>
          if maze.advance(tracer, state.costs) then
            val tracers = state.tracers ++ tracer.options
            val costs   = state.costs + (tracer.pos -> tracer.dir -> tracer.cost)
            State(tracers = tracers, costs = costs)
          else
            state

      def solution: Option[Long] =
        when(tracers.isEmpty)(cost)

      def cost: Long =
        values.flatMap(d => costs.get(maze.end -> d)).min

    val tracer = Tracer(cost = 0, pos = maze.start, dir = E, path = Set(maze.start))
    val state  = State(Vector(tracer))
    iterate(state)(_.next).findMap(_.solution)


  def solve2(maze: Maze): Long =

    case class Best(cost: Long, path: Set[Pos])
    case class State(tracers: Vector[Tracer], costs: Costs, best: Best):

      def update(tracer: Tracer, cost: Costs, best: Best): Best =
        if      tracer.pos != maze.end || tracer.cost > best.cost then best
        else if tracer.cost < best.cost                           then Best(tracer.cost, tracer.path)
        else                                                           best.copy(path = best.path ++ tracer.path)

      def next: State =
        tracers.foldLeft(copy(tracers = Vector.empty[Tracer])): (state, tracer) =>
          val best    = update(tracer, state.costs, state.best)
          val tracers = tracer.options.filter(tracer => maze.advance(tracer, state.costs))
          val costs   = state.costs ++ tracers.map(next => next.pos -> next.dir -> next.cost)
          State(tracers = state.tracers ++ tracers, costs = costs, best = best)

      def solution: Option[Long] =
        when(tracers.isEmpty)(best.path.size)

    val tracers = Vector(Tracer(0, maze.start, Dir.E, Set(maze.start)))
    val costs   = Map(maze.start -> E -> 0L)
    val best    = Best(Long.MaxValue, Set.empty)
    val state   = State(tracers, costs, best)
    iterate(state)(_.next).findMap(_.solution)

  val maze: Maze = Maze(grid = Grid.fromLines(lines))

  override lazy val answer1: Long = solve1(maze)
  override lazy val answer2: Long = solve2(maze)
