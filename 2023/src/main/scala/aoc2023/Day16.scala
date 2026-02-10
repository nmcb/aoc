package aoc2023

import nmcb.*
import nmcb.pos.{*, given}

import scala.annotation.tailrec

object Day16 extends AoC:

  case class Grid(grid: Vector[Vector[Char]]):
    private val sizeX: Int = grid.map(_.size).max
    private val sizeY: Int = grid.size

    private def tile(p: Pos): Char =
      grid(p.y)(p.x)

    private def advance(p: Pos, d: Dir): List[(Pos,Dir)] =
      val result: List[(Pos,Dir)] =
        (tile(p), d) match
          case ('.', N)  => List(p.step(N) -> N)
          case ('.', E)  => List(p.step(E) -> E)
          case ('.', S)  => List(p.step(S) -> S)
          case ('.', W)  => List(p.step(W) -> W)

          case ('|', N)  => List(p.step(N) -> N)
          case ('|', E)  => List(p.step(N) -> N, p.step(S) -> S)
          case ('|', S)  => List(p.step(S) -> S)
          case ('|', W)  => List(p.step(N) -> N, p.step(S) -> S)

          case ('-', N)  => List(p.step(W) -> W, p.step(E) -> E)
          case ('-', E)  => List(p.step(E) -> E)
          case ('-', S)  => List(p.step(W) -> W, p.step(E) -> E)
          case ('-', W)  => List(p.step(W) -> W)

          case ('/', N)  => List(p.step(E) -> E)
          case ('/', E)  => List(p.step(N) -> N)
          case ('/', S)  => List(p.step(W) -> W)
          case ('/', W)  => List(p.step(S) -> S)

          case ('\\', N) => List(p.step(W) -> W)
          case ('\\', E) => List(p.step(S) -> S)
          case ('\\', S) => List(p.step(E) -> E)
          case ('\\', W) => List(p.step(N) -> N)

          case _ => sys.error(s"advancing: p=$p, d=$d")

      result.filter((p,_) => p.x >= 0 && p.y >= 0 && p.x < sizeX && p.y < sizeY)

    private def trace(start: Pos, direction: Dir): Set[(Pos,Dir)] =
      @tailrec
      def loop(todo: List[(Pos,Dir)], acc: Set[(Pos,Dir)] = Set.empty): Set[(Pos,Dir)] =
        todo match
          case Nil                                  => acc
          case (p,d) :: rest if acc.contains((p,d)) => loop(rest, acc)
          case (p,d) :: rest                        =>
            advance(p, d) match
              case Nil => loop(rest, acc + (p -> d))
              case hs  => loop(hs ++ rest, acc + (p -> d))

      loop(List(start -> direction)) ++ Set(start -> direction)

    def energized(start: Pos, direction: Dir): Int =
      trace(start, direction).toMap.keys.size

    def maxEnergized: Int =
      val north = List.tabulate(sizeX)(x => Pos.of(x, 0) -> S)
      val east  = List.tabulate(sizeY)(y => Pos.of(sizeX - 1, y) -> W)
      val south = List.tabulate(sizeX)(x => Pos.of(x, sizeY - 1) -> N)
      val west  = List.tabulate(sizeY)(y => Pos.of(0, y) -> E)
      val entries = List(north, east, south, west).flatten
      entries.map((p,d) => energized(p,d)).max

  lazy val grid: Grid = Grid(lines.map(_.toVector))

  override lazy val answer1: Long = grid.energized(start = Pos.of(0,0), direction = E)
  override lazy val answer2: Long = grid.maxEnergized
