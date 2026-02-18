package aoc2024

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day10 extends AoC:

  val grid: Grid[Int] = Grid.fromLines(lines).map(_.asDigit)

  type TrailHead = Set[Vector[Pos]]

  extension (trails: TrailHead)
    
    def reachableSummits: Iterable[Int] =
      trails.groupMap(_.head)(_.last).values.map(_.size)


  extension (g: Grid[Int])

    def startingPoints: Set[Pos] =
      g.filter((_, i) => i == 0).map(_.pos)

    def trailsFrom(head: Pos): TrailHead =

      def step(trail: Vector[Pos]): TrailHead =
        trail.last.adjoint4
          .filter(n => g.within(n) && g.peek(n) == g.peek(trail.last) + 1)
          .map(p => trail :+ p)

      @tailrec
      def loop(trails: TrailHead, current: Int): TrailHead =
        val result = trails.filter(trail => g.peek(trail.last) == current)
        if current >= 9 then result else loop(result.flatMap(step), current + 1)

      loop(Set(Vector(head)), 0)

  def score(grid: Grid[Int]): Long =
    grid.startingPoints.flatMap(grid.trailsFrom).reachableSummits.sum

  def rating(grid: Grid[Int]): Long =
    grid.startingPoints.flatMap(grid.trailsFrom).size


  override lazy val answer1: Long = score(grid)
  override lazy val answer2: Long = rating(grid)
