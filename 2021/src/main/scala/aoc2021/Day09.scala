package aoc2021

import nmcb.*
import nmcb.predef.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day09 extends AoC:

  val floor: Grid[Int] = Grid.fromLines(lines).map(_.toString.toInt)

  type PosHeight = (Pos, Int)

  extension (floor: Grid[Int])

    def neighbouringHeights(p: Pos): Set[PosHeight] =
      p.adjoint4.flatMap(p => floor.peekOption(p).map(h => p -> h))

    def upstream(p: Pos): Set[Pos] =
      neighbouringHeights(p).filter(ph => ph.right < 9 && ph.right > floor.peek(p)).map(_.element)

    def posHeights: Vector[PosHeight] =
      floor.elements.foldLeft(Vector.empty[PosHeight]):
        case (result, ph) if floor.neighbouringHeights(ph.left).forall(nh => ph.right < nh.right) => ph +: result
        case (result, _)                                                                          => result

  def solve1(floor: Grid[Int]): Int =
    floor.posHeights.map(ph => ph.right + 1).sum

  def solve2(floor: Grid[Int]): Int =
    def basinSize(p: Pos): Int =
      @tailrec
      def loop(todo: Set[Pos], result: Set[Pos] = Set.empty): Int =
        if todo.isEmpty then
          result.size
        else
          val current  = todo.head
          val next     = todo.tail
          val upstream = floor.upstream(current)
          loop(upstream ++ next, result + current)
      loop(Set(p))
    floor.positions.map(basinSize).toVector.sorted.reverse.take(3).product


  override lazy val answer2: Int = solve2(floor)
  override lazy val answer1: Int = solve1(floor)
