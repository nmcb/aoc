package aoc2020

import nmcb.*

import scala.collection.Iterator.iterate

object Day25 extends AoC:

  val divider = 20201227
  
  def step(n: Int): Int =
    (n * 7) % divider

  def solve(card: Int, door: Int): Int =
    BigInt(card).modPow(iterate(1)(step).indexWhere(_ == door), divider).toInt

  override lazy val answer1: Int = solve(card = 5290733, door = 15231938)
