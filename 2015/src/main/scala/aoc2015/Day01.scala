package aoc2015

import nmcb.*
import nmcb.predef.*

object Day01 extends AoC:

  val commands: List[Char] = input.toList

  lazy val answer1: Int =
    commands
      .foldLeft(0): (f,c) =>
        if c == '(' then
          f + 1
        else if c == ')' then
          f - 1
        else
          sys.error(s"unknown command: '$c'")
  
  lazy val answer2: Int =
    commands
      .scanLeft(0): (f,c) =>
        if c == '(' then
          f + 1
        else if c == ')' then
          f - 1
        else
          sys.error(s"unknown command: '$c'")
      .zipWithIndex
      .find((f,_) => f == -1)
      .getOrElse(sys.error("not found"))
      .index
