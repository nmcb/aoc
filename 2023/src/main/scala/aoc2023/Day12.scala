package aoc2023

import nmcb.*
import nmcb.predef.*

import scala.collection.*

object Day12 extends AoC:

  case class Line(mask: String, lengths: Vector[Int]):

    def unfold: Line =
      Line(Vector.fill(5)(mask).reduce(_ ++ "?" ++ _), Vector.fill(5)(lengths).reduce(_ ++ _))

  object Line:

    def fromString(s: String): Line =
      val Array(mask, groups) = s.split(' ')
      Line(mask.trim, groups.trim.split(',').map(_.toInt).toVector)

  case class Puzzle(lines: Vector[Line]):

    val cache: mutable.Map[(String, Vector[Int]), Long] =
      memo[(String, Vector[Int]), Long]()

    private def solve(mask: String, lengths: Vector[Int]): Long =
      cache.memoize((mask, lengths)):
        (mask, lengths).runtimeChecked match
          case (_, Vector()) if mask.contains('#')                  => 0
          case (_, Vector())                                        => 1
          case ("", _ +: _)                                         => 0
          case (s".$tail", _)                                       => solve(tail, lengths)
          case (s"#$tail", length +: todo) if mask.length >= length =>
            val (operational, rest) = tail.splitAt(length - 1)
            if operational.contains('.') then
              0
            else
              rest match
                case ""      => solve("", todo)
                case s".$mt" => solve(mt, todo)
                case s"?$mt" => solve(mt, todo)
                case _       => 0
          case (s"#$tail", _ +: _) => 0
          case (s"?$tail", _)      => solve(s".$tail", lengths) + solve(s"#$tail", lengths)

    lazy val arrangements: Long =
      lines.map(l => solve(l.mask, l.lengths)).sum

    lazy val unfoldAll: Puzzle =
      Puzzle(lines.map(_.unfold))

  lazy val puzzle: Puzzle = Puzzle(lines.map(Line.fromString))


  override lazy val answer1: Long = puzzle.arrangements
  override lazy val answer2: Long = puzzle.unfoldAll.arrangements
