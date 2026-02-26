package aoc2022

import nmcb.*
import nmcb.pos.*

object Day08 extends AoC:

  extension (grid: Grid[Int])

    def isBorder(p: Pos): Boolean =
      p.x == 0 || p.x == grid.maxPos.x | p.y == 0 || p.y == grid.maxPos.y

    def visible(p: Pos): Boolean =
      def pathL(p: Pos): Vector[Int] = (0 to p.x).map(x             => grid.peek((x, p.y))).toVector
      def pathR(p: Pos): Vector[Int] = (p.x to grid.maxPos.x).map(x => grid.peek((x, p.y))).toVector.reverse
      def pathT(p: Pos): Vector[Int] = (0 to p.y).map(y             => grid.peek((p.x, y))).toVector
      def pathB(p: Pos): Vector[Int] = (p.y to grid.maxPos.y).map(y => grid.peek((p.x, y))).toVector.reverse
      def visible(todo: Vector[Int]): Boolean = todo.init.max < todo.last
      val vl = visible(pathL(p))
      val vr = visible(pathR(p))
      val vt = visible(pathT(p))
      val vb = visible(pathB(p))
      vl || vr || vt || vb

    def maxVisibleTrees: Int =
      grid.positions.foldLeft(0): (c, p) =>
        if      isBorder(p) then c + 1
        else if visible(p)  then c + 1
        else                     c

    def scenicScore(p: Pos): Int =
      def pathL(p: Pos): Vector[Int] = (0 to p.x).map(x             => grid.peek((x, p.y))).toVector.reverse
      def pathR(p: Pos): Vector[Int] = (p.x to grid.maxPos.x).map(x => grid.peek((x, p.y))).toVector
      def pathT(p: Pos): Vector[Int] = (0 to p.y).map(y             => grid.peek((p.x, y))).toVector.reverse
      def pathB(p: Pos): Vector[Int] = (p.y to grid.maxPos.y).map(y => grid.peek((p.x, y))).toVector
      def score(todo: Vector[Int]): Int =
        val index  = todo.tail.indexWhere(_ >= todo.head)
        if index != -1 then index + 1 else todo.tail.length
      val vl = score(pathL(p))
      val vr = score(pathR(p))
      val vt = score(pathT(p))
      val vb = score(pathB(p))
      vl * vr * vt * vb

    def maxScenicScore: Int =
      grid.positions.map(scenicScore).max

  private val grid: Grid[Int] = Grid.fromLines(lines).map(_.asDigit)


  override lazy val answer1: Long = grid.maxVisibleTrees
  override lazy val answer2: Long = grid.maxScenicScore
