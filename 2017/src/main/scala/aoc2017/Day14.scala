package aoc2017

import nmcb.*
import nmcb.pos.*

import Day10.KnotHash
import Day12.Graph

import scala.annotation.tailrec

object Day14 extends AoC:

  extension (knot: KnotHash)

    def toBinary: Vector[Int] =
      knot
        .denseHash
        .flatMap: bin =>
          Vector(bin >> 4 & 0x0f, bin & 0x0f)
        .foldLeft(Vector.empty[Int]): (result, bin) =>
          result ++ Vector(bin >> 3 & 0x01, bin >> 2 & 0x01, bin >> 1 & 0x01, bin >> 0 & 0x01)

    def toBinaryString: String =
      knot.toBinary.mkString("")

  val hashes: IndexedSeq[String] =
    for i <- 0 until 128 yield KnotHash.compute(s"uugsqrei-$i", runs = 64).toBinaryString

  extension (pos: Pos)
    def adjacent: Set[Pos] =
      pos.adjoint4.filter(p => p.x >= 0 && p.x < 128 && p.y >= 0 && p.y < 128)

  val used: Set[Pos] =
    hashes
      .map(_.zipWithIndex)
      .zipWithIndex
      .foldLeft(Set.empty[Pos]):
        case (result, (row, y)) =>
          row.foldLeft(result):
            case (result, (square, x)) =>
              if square == '1' then result + Pos.of(x, y) else result

  def regions(used: Set[Pos]): Set[Set[Pos]] =
    val graph: Graph[Pos] =
      used
        .map: square =>
          square -> square.adjacent.intersect(used)
        .toMap
    
    @tailrec
    def loop(todo: Set[Pos], regions: Set[Set[Pos]] = Set.empty): Set[Set[Pos]] =
      if todo.isEmpty then
        regions
      else
        val square = todo.head
        val region = Dijkstra.reachable(square, graph)
        loop(todo -- region - square, regions + region)
    loop(used)

  override lazy val answer1: Int = hashes.mkString("").count(_ == '1')
  override lazy val answer2: Int = regions(used).size
