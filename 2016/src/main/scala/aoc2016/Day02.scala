package aoc2016

import nmcb.*

object Day02 extends AoC:

  case class Line(instructions: Vector[Char])

  object Line:
    def fromString(s: String): Line =
      Line(s.toVector)

  val puzzle: Vector[Line] = lines.map(Line.fromString)

  case class Pad(pad: Vector[String], x: Int, y: Int):

    val current: Char =
      pad(y)(x)

    infix def advance(i: Char): Pad =
      val (nx, ny) =
        i match
          case 'U' => (x, y - 1)
          case 'D' => (x, y + 1)
          case 'L' => (x - 1, y)
          case 'R' => (x + 1, y)
      if pad(ny)(nx) != ' ' then copy(x = nx, y = ny) else this

    infix def process(l: Line): Pad =
      l.instructions.foldLeft(this)(_ advance _)

  object Pad:

    def pad1: Pad =
      Pad(
        pad =
          Vector(
            "     ",
            " 123 ",
            " 456 ",
            " 789 ",
            "     "
          ),
        x = 2,
        y = 2)

    def pad2: Pad =
      Pad(
        pad =
          Vector(
            "       ",
            "   1   ",
            "  234  ",
            " 56789 ",
            "  ABC  ",
            "   D   ",
            "       "
          ),
        x = 1,
        y = 3)

  def solve(pad: Pad, lines: Vector[Line]): String =
    lines.scanLeft(pad)(_ process _).map(_.current).tail.mkString

  lazy val answer1: String = solve(Pad.pad1, puzzle)
  lazy val answer2: String = solve(Pad.pad2, puzzle)
