package aoc2016

import nmcb.*

import scala.annotation.tailrec
import scala.util.*
import scala.util.matching.Regex

object Day09 extends AoC:

  val Marker: Regex = """\((\d+)x(\d+)\)""".r

  def solve1(str: String): Long =
    @tailrec
    def loop(todo: String, result: Long): Long =
      Marker.findFirstMatchIn(todo) match
        case Some(m) =>
          val length = m.group(1).toInt
          val times  = m.group(2).toInt
          loop(m.after.toString.substring(length), result + m.start + times * length)
        case None =>
          result + todo.length
    loop(str, 0L)

  def solve2(str: String): Long =

    case class State(todo: String = str, multiplier: Long = 1L)

    @tailrec
    def loop(stack: List[State], result: Long): Long =
      stack match
        case Nil =>
          result
        case State(todo, multiplier) :: rest =>
          Marker.findFirstMatchIn(todo) match
            case Some(m) =>
              val length    = m.group(1).toInt
              val times     = m.group(2).toLong
              val after     = m.after.toString
              val process   = after.substring(0, length)
              val remainder = after.substring(length)
              val addition  = m.start * multiplier
              val next      = State(process, multiplier * times) :: State(remainder, multiplier) :: rest
              loop(next, result + addition)
            case None =>
              loop(rest, result + todo.length * multiplier)
    loop(List(State()), 0L)

  override lazy val answer1: Long = solve1(input)
  override lazy val answer2: Long = solve2(input)
