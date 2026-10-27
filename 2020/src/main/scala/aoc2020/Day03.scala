package aoc2020

import nmcb.*

import scala.Option.when
import scala.annotation.tailrec


object Day03 extends AoC:

  extension (grid: Grid[Char])

    def horizontalStretchedPeek(x: Int, y: Int): Option[Char] =
      when(y < grid.sizeY)(grid.peek(x % grid.sizeX, y))

    def walk(dx: Int, dy: Int): Long =
      @tailrec
      def step(x: Int, y: Int, result: Long = 0): Long =
        horizontalStretchedPeek(x, y).runtimeChecked match
          case None      => result
          case Some('.') => step(x + dx, y + dy, result)
          case Some('#') => step(x + dx, y + dy, result + 1)
      step(dx,dy)

  val forest: Grid[Char] = Grid.fromLines(lines)

  val walks: List[Long] =
    List(
      forest.walk(1, 1),
      forest.walk(3, 1),
      forest.walk(5, 1),
      forest.walk(7, 1),
      forest.walk(1, 2)
    )


  override lazy val answer1: Long = forest.walk(3, 1)
  override lazy val answer2: Long = walks.product
