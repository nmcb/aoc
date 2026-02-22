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


  /** part 2 - utilizes the input grid being square and the solution scaling quadratically in grid size */

  /** provides for an infinite garden using the grid as tiles */
  extension (tile: Grid[Char])

    def size: Int =
      assert(tile.sizeX == tile.sizeY)
      tile.sizeX

    def peekInfinite(p: Pos): Char =
      val wrapX: Int = (p.x % tile.sizeX + tile.sizeX) % tile.sizeX
      val wrapY: Int = (p.y % tile.sizeY + tile.sizeY) % tile.sizeY
      tile.peek(wrapX, wrapY)


  case class InfiniteGarden(tile: Grid[Char], steps: Int, plots: Set[Pos]):

    def next: InfiniteGarden =
      copy(
        steps = steps + 1,
        plots =
          for
            plot <- plots
            step <- plot.adjoint4
            if tile.peekInfinite(step) != '#'
          yield
            step
      )

  object InfiniteGarden:

    def from(garden: Grid[Char]): InfiniteGarden =
      InfiniteGarden(tile = garden, steps = 0, plots = Set(garden.findOne('S')))


  case class Collector(tile: Grid[Char], steps: Long, tilePlotScan: Vector[Long] = Vector.empty):

    val stepsToTiles: Long = steps / tile.size
    val tilesToSteps: Long = steps % tile.size

    infix def add(garden: InfiniteGarden): Collector =
      if garden.steps % tile.size == tilesToSteps then
        copy(tilePlotScan = tilePlotScan :+ garden.plots.size)
      else
        this

    /** given three distinct subsequent `y` points on a quadratic polynomial, solve `y` for given `x` */
    def quadratic(y0: Long, y1: Long, y2: Long)(x: Long): Long =
      y0 + (y1 - y0) * x + (x * (x - 1) / 2) * (y2 - 2 * y1 + y0)

    def solution: Option[Long] =
      tilePlotScan match
        case Vector(plots0, plots1, plots2) => Some(quadratic(plots0, plots1, plots2)(stepsToTiles))
        case _                              => None

  object Collector:

    def from(tile: Grid[Char], steps: Long): Collector =
      Collector(tile = tile, steps = steps, tilePlotScan = Vector.empty)


  def solve2(garden: Grid[Char], steps: Long): Long =
    Iterator
      .iterate(InfiniteGarden.from(garden))(_.next)
      .scanLeft(Collector.from(garden, steps))(_ add _)
      .flatMap(_.solution)
      .next


  val garden: Grid[Char] = Grid.fromLines(lines)

  override lazy val answer1: Long = solve1(garden, 64)
  override lazy val answer2: Long = solve2(garden, 26501365)
