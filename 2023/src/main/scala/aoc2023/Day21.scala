package aoc2023

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

/** @see Credits for part 2 - https://github.com/merlinorg */
object Day21 extends AoC:

  extension (garden: Grid[Char])

    def rock(p: Pos): Boolean =
      garden.peekOption(p).contains('#')

    def plot(p: Pos): Boolean =
      garden.peekOption(p).exists(c => c == '.' || c == 'S')

  /** part 1 - simple bfs */

  def solve1(garden: Grid[Char], steps: Long): Long =

    @tailrec
    def loop(count: Long, found: Set[Pos] = Set(garden.findOne('S'))): Set[Pos] =
      if count == 0 then
        found
      else
        loop(count - 1, found.flatMap(_.adjoint4.filter(garden.plot)))

    loop(steps).size


  /** part 2 - utilises the input grid being square and the solution scaling quadratically in grid size */

  extension (garden: Grid[Char])

    def gridSize: Int =
      assert(garden.sizeX == garden.sizeY)
      garden.sizeY

    /** provides for an infinite garden using this grid as tiles */
    def infinite(p: Pos): Char =
      val wrapX: Int = (p.x % garden.sizeX + garden.sizeX) % garden.sizeX
      val wrapY: Int = (p.y % garden.sizeY + garden.sizeY) % garden.sizeY
      garden.peek(wrapX, wrapY)

  case class Garden(grid: Grid[Char], steps: Int, plots: Set[Pos]):

    def next: Garden =
      copy(
        steps = steps + 1,
        plots =
          for
            plot <- plots
            step <- plot.adjoint4
            if grid.infinite(step) != '#'
          yield
            step
      )

  object Garden:
    def make(grid: Grid[Char]): Garden =
      Garden(grid = grid, steps = 0, plots = Set(grid.findOne('S')))


  case class Collect(grid: Grid[Char], steps: Long, gridPlotsScan: Vector[Long] = Vector.empty):
    val stepsToGrids: Long = steps / grid.gridSize
    val gridsToSteps: Long = steps % grid.gridSize

    infix def add(garden: Garden): Collect =
      if garden.steps % grid.gridSize == gridsToSteps then
        copy(gridPlotsScan = gridPlotsScan :+ garden.plots.size)
      else
        this

    /** given three distinct subsequent `y` points on a quadratic polynomial, solve `y` for given `x` */
    def quadratic(y0: Long, y1: Long, y2: Long)(x: Long): Long =
      y0 + (y1 - y0) * x + (x * (x - 1) / 2) * (y2 - 2 * y1 + y0)

    def solution: Option[Long] =
      gridPlotsScan match
        case Vector(plots0, plots1, plots2) => Some(quadratic(plots0, plots1, plots2)(stepsToGrids))
        case _                              => None

  def solve2(grid: Grid[Char], steps: Int): Long =
    Iterator
      .iterate(Garden.make(grid))(_.next)
      .scanLeft(Collect(grid, steps))(_ add _)
      .flatMap(_.solution)
      .next

  val garden: Grid[Char] = Grid.fromLines(lines)


  override lazy val answer1: Long = solve1(garden, 64)
  override lazy val answer2: Long = solve2(garden, 26501365)
