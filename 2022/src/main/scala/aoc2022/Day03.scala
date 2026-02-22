package aoc2022

import nmcb.*

import scala.io.*

object Day03 extends AoC:

  def priority(c: Char): Int =
    if c.isLower then c.toInt - 96 else c.toInt - 38

  def priority1(line: String): Int =
    val left  = line.take(line.length / 2).toSet
    val right = line.takeRight(line.length / 2).toSet
    val char  = (left intersect right).head
    priority(char)

  def priority2(lines: Vector[String]): Int =
    val Vector(r1, r2, r3) = lines.map(_.toSet)
    val char               = (r1 intersect r2 intersect r3).head
    priority(char)


  override lazy val answer1: Int = lines.map(priority1).sum
  override lazy val answer2: Int = lines.grouped(3).map(priority2).sum
