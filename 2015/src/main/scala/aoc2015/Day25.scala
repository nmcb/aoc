package aoc2015

import nmcb.*

object Day25 extends AoC:

  def indexOf(row: Int, column: Int): Int =
    val diagonal = row + column - 2
    val triangle = (1 to diagonal).sum
    triangle + column

  val initial: BigInt  = BigInt(20151125)
  val base: BigInt     = BigInt(252533)
  val modulo: BigInt   = BigInt(33554393)
  val exponent: Int    = indexOf(row = 2947, column = 3029) - 1

  override lazy val answer1: BigInt = initial * base.modPow(exponent, modulo) % modulo
