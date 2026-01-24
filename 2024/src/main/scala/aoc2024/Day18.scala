package aoc2024

import nmcb.*
import nmcb.pos.*

import scala.annotation.*

object Day18 extends AoC:

  import Dijkstra.*
  import Pos.*

  val bytes: Vector[Pos] = lines.map(_.split(',')).map(_.toPos)
  val memory: Grid[Char] = Grid.fill(71, 71, '.')

  override lazy val answer1: Int =
    val fallen = bytes.take(1024).foldLeft(memory)(_.updated(_, '#'))
    val graph  = Graph.fromGrid(fallen, '.')
    val result = Dijkstra.run(fallen.minPos, graph)
    result.distanceTo(fallen.maxPos).get

  @tailrec
  def loop(todo: Vector[Pos], grid: Grid[Char]): Pos =
    val test     = grid.updated(todo.head, '.')
    val graph    = Graph.fromGrid(test, '.')
    val distance = Dijkstra.run(test.minPos, graph).distanceTo(test.maxPos)
    if distance.isEmpty then loop(todo.tail, test) else todo.head

  override lazy val answer2: Pos = loop(bytes.reverse, bytes.foldLeft(memory)(_.updated(_, '#')))
