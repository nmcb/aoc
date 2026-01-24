package aoc2016

import nmcb.*

object Day18 extends AoC:

  def next(row: Vector[Char]): Vector[Char] =
    ('.' +: row :+ '.')
      .sliding(3)
      .map:
        case Vector('^', '^', '.') | Vector('.', '^', '^') | Vector('^', '.', '.') | Vector('.', '.', '^') => '^'
        case _                             => '.'
      .toVector

  def safe(row: Vector[Char]): Int =
    row.count(_ == '.')

  override lazy val answer1: Int = Iterator.iterate(input.toVector)(next).map(safe).take(40).sum
  override lazy val answer2: Int = Iterator.iterate(input.toVector)(next).map(safe).take(400000).sum
