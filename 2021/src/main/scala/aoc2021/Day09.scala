package aoc2021

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day09 extends AoC:

  val floor: Grid[Int] = Grid.fromLines(lines).map(_.toString.toInt)

  extension (floor: Grid[Int])

    def neighbouringHeights(p: Pos): Set[(Pos, Int)] =
      p.adjoint4.flatMap(p => floor.peekOption(p).map(h => p -> h))

    def upstream(p: Pos): Set[Pos] =
      neighbouringHeights(p).filter((_, nh) => nh < 9 && nh > floor.peek(p)).map((n, _) => n)

    def heights: List[(Pos, Int)] =
      floor.elements.foldLeft(List.empty[(Pos,Int)]):
        case (result, (p, h)) if floor.neighbouringHeights(p).forall((_, nh) => h < nh) => (p, h) :: result
        case (result, _)                                                                => result


  lazy val answer1: Int =
    floor.heights.map((_, h) => h + 1).sum


  lazy val answer2: Int =
    @tailrec
    def loop(todo: Set[Pos], acc: Set[Pos] = Set.empty): Int =
      if todo.isEmpty then
        acc.size
      else
        val current  = todo.head
        val upstream = floor.upstream(current)
        loop(upstream ++ todo.tail, acc + current)
    
    floor.heights.map((p, _) => loop(Set(p))).sorted.reverse.take(3).product
