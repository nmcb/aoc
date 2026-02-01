package aoc2015

import nmcb.*
import nmcb.predef.*

object Day01 extends AoC:

  val commands: Vector[Char] = input.toVector
  
  def countOpenOrClose(counter: Int, char: Char): Int =
      if char == '(' then
        counter + 1
      else if char == ')' then
        counter - 1
      else
        sys.error(s"unknown command: '$char'")

  override lazy val answer1: Int =
    commands.foldLeft(0)(countOpenOrClose) 
  
  override lazy val answer2: Int =
    commands.scanLeft(0)(countOpenOrClose).zipWithIndex.findFirst(_.element == -1).index
