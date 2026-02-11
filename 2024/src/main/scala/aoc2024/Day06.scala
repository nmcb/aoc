package aoc2024

import nmcb.*
import nmcb.pos.{*, given}

import scala.annotation.*

object Day06 extends AoC:

  val grid: Grid[Char] = Grid.fromLines(lines)

  extension (g: Grid[Char])

    @tailrec
    def walkGuard(pos: Pos, dir: Dir, result: Set[Pos] = Set.empty): Set[Pos] =
      val next = pos step dir
      g.peekOrElse(next, ' ') match
        case ' ' => result + pos
        case '.' => walkGuard(next, dir, result + pos)
        case '#' => walkGuard(pos, dir.cw, result)

    def peekWithObstruction(p: Pos, obstruct: Pos)(using CanEqual[Pos, Pos]): Char =
        if p == obstruct then '#' else g.peekOrElse(p, ' ')

    @tailrec
    def walkCircular(pos: Pos, dir: Dir, obstruct: Pos, visited: Set[(Pos,Dir)] = Set.empty): Boolean =
      if visited.contains((pos, dir)) then
        true
      else
        val next = pos step dir
        g.peekWithObstruction(next, obstruct) match
          case ' ' => false
          case '.' => walkCircular(next, dir, obstruct, visited + (pos -> dir))
          case '#' => walkCircular(pos, dir.cw, obstruct, visited)


  lazy val start: Pos          = grid.findOne('^')
  lazy val cleared: Grid[Char] = grid.map(c => if c == '^' then '.' else c)
  override lazy val answer1: Int = cleared.walkGuard(start, N).size

  def obstructions: Set[Pos] = cleared.walkGuard(start, N) - start
  override lazy val answer2: Int = obstructions.count(o => cleared.walkCircular(start, N, o))
