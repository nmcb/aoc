package aoc2017

import nmcb.*

object Day09 extends AoC:

  private var garbage: Boolean = false
  private var escape: Boolean  = false

  private var depth: Int   = 0
  private var score: Int   = 0
  private var counter: Int = 0

  input.foreach: char =>
    if garbage then if      escape      then escape   = false
                    else if char == '>' then garbage  = false
                    else if char == '!' then escape   = true
                    else                     counter += 1
    else if char == '{' then { depth += 1 ; score += depth }
    else if char == '}' then   depth -= 1
    else if char == '<' then   garbage = true

  override lazy val answer1: Int = score
  override lazy val answer2: Int = counter
