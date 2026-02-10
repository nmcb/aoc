package aoc2021

import nmcb.*
import nmcb.pos.{*, given}

object Day13 extends AoC:

  enum Axis:
    case Ver, Hor

  import Axis.*
  
  given CanEqual[Axis, Axis] = CanEqual.derived

  type Dots  = Set[Pos]
  type Folds = Vector[(Axis, Int)]

  val dots: Dots =
    lines
      .collect:
        case s"$x,$y" => (x.toInt, y.toInt)
      .toSet
      
  val folds: Folds =
    lines
      .collect:
        case s"fold along x=$x" => (Ver, x.toInt)
        case s"fold along y=$y" => (Hor, y.toInt)
      .toVector

  def origami(dots: Dots, folds: Folds): Dots =

    def vertical(line: Int)(p: Pos): Pos =
      (x = if p.x < line then p.x else 2 * line - p.x, y = p.y)

    def horizontal(line: Int)(p: Pos): Pos =
      (x = p.x, y = if p.y < line then p.y else 2 * line - p.y)

    folds.foldLeft(dots):
      case (dots, (Ver, line)) => dots.map(vertical(line))
      case (dots, (Hor, line)) => dots.map(horizontal(line))

  extension (dots: Dots) def asString: String =
    val buffer = StringBuffer()
    for y <- 0 to dots.map(_.y).max do
      buffer.append('\n')
      for x <- 0 to dots.map(_.x).max do
        if dots.contains((x,y)) then
          buffer.append('#')
        else
          buffer.append('.')
    buffer.toString + "\n"

  override lazy val answer1: Int    = origami(dots, folds.take(1)).size
  override lazy val answer2: String = origami(dots, folds).asString
