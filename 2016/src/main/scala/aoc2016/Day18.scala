package aoc2016

import nmcb.*

object Day18 extends AoC:

  def next(row: String): String =
    ("." + row + ".")
      .sliding(3)
      .map:
        case "^^." | ".^^" | "^.." | "..^" => '^'
        case _                             => '.'
      .mkString

  def safe(row: String): Int =
    row.count(_ == '.')

  lazy val answer1: Int = Iterator.iterate(input)(next).map(safe).take(40).sum
  lazy val answer2: Int = Iterator.iterate(input)(next).map(safe).take(400000).sum
