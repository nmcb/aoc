package aoc2022

import nmcb.*

import scala.annotation.tailrec
import scala.util.*

object Day05 extends AoC:

  case class Move(size: Int, from: Int, to: Int)

  def isLabelLine(line: String): Boolean =
    line.trim.nonEmpty && line.trim.head.isDigit

  def isStackLine(line: String): Boolean =
    line.dropWhile(_ == ' ').startsWith("[")

  def isCommandLine(line: String): Boolean =
    line.startsWith("move")

  val stack: Vector[Vector[Char]] =
    
    val columnSize: Int =
      lines.filter(isLabelLine).runtimeChecked match
        case Vector(cols) => cols.split(' ').last.toInt
        
    def crateExtractor(line: String)(idx: Int): Option[Char] =
      Try(line.charAt(1 + idx * 4)) match
        case Success(c) if c.isUpper => Some(c)
        case _                       => None
        
    def parser(line: String): Vector[Option[Char]] =
      Vector.tabulate(columnSize)(crateExtractor(line))
      
    lines
      .filter(isStackLine)
      .map(parser)
      .transpose
      .map(_.flatten)

  val moves: Vector[Move] = lines.filter(isCommandLine).map:
    case s"move $s from $f to $t" => Move(s.toInt, f.toInt - 1, t.toInt - 1)


  @tailrec
  def run(moves: Vector[Move], stacks: Vector[Vector[Char]], place: Vector[Char] => Vector[Char]): String =
    if moves.isEmpty then
      stacks.map(_.head).mkString("")
    else
      val move    = moves.head
      val crates  = stacks(move.from).take(move.size)
      val take    = stacks(move.from).drop(move.size)
      val replace = place(crates) ++ stacks(move.to)
      val next    = stacks.updated(move.from, take).updated(move.to, replace)
      run(moves.tail, next, place)


  override lazy val answer1: String = run(moves, stack, _.reverse)
  override lazy val answer2: String = run(moves, stack, identity)
