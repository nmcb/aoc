package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day14 extends AoC:

  extension (grid: Grid[Char])

    private def tiltLeft(row: Vector[Char]): Vector[Char] =
      @tailrec
      def loop(todo: Vector[Char], acc: Vector[Char] = Vector.empty): Vector[Char] =
        if todo.isEmpty then
          acc
        else if todo.head == '#' then
          loop(todo.tail, acc :+ '#')
        else
          val section = todo.takeWhile(_ != '#')
          val rounds  = section.count(_ == 'O')
          val empties = section.count(_ == '.')
          val aligned = section.foldLeft(Seq.empty[Char])((a, p) => Seq.fill(rounds)('O') ++ Seq.fill(empties)('.'))
          loop(todo.drop(section.length), acc ++ aligned)

      loop(row)

    def tiltW: Grid[Char] =
      Grid(matrix = grid.matrix.map(tiltLeft))

    def tiltE: Grid[Char] =
      grid.flipX.tiltW.flipX

    def tiltN: Grid[Char] =
      grid.transpose.tiltW.transpose

    def tiltS: Grid[Char] =
      grid.transpose.flipX.tiltW.flipX.transpose

    def cycle: Grid[Char] =
      tiltN.tiltW.tiltS.tiltE

    def load: Int =
      grid.matrix.reverse.zipWithIndex.foldLeft(0):
        case (a, (r, i)) => a + r.count(_ == 'O') * (i + 1)


  lazy val grid: Grid[Char] = Grid.fromLines(lines)

  override lazy val answer1: Long = grid.tiltN.load
  override lazy val answer2: Long = Cycle.find(grid, _.cycle).simulate(1_000_000_000L).load
