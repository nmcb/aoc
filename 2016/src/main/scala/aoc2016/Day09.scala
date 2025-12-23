package aoc2016

import nmcb.*

import scala.util.*
import scala.util.matching.Regex

object Day09 extends AoC:

  val marker: Regex = """\((\d+)x(\d+)\)""".r

  def solve1(str: String): Long =
    marker.findFirstMatchIn(str) match
      case Some(m) =>
        val length = m.group(1).toInt
        val times  = m.group(2).toInt
        m.start + times * length + solve1(m.after.toString.substring(length))
      case None =>
        str.length

  def solve2(str: String): Long =
    marker.findFirstMatchIn(str) match
      case Some(m) =>
        val length  = m.group(1).toInt
        val times   = m.group(2).toInt
        val after   = m.after.toString
        val process = after.substring(0, length)
        m.start + times * solve2(process) + solve2(after.substring(length))
      case None =>
        str.length

  lazy val answer1: Long = solve1(input)
  lazy val answer2: Long = solve2(input)
