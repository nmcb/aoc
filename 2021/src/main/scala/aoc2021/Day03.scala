package aoc2021

import nmcb.*
import scala.annotation.*

object Day03 extends AoC:

  val Zero = '0'
  val One  = '1'

  type Bits = Vector[Char]

  val diagnostics: Vector[Bits] = lines.map(_.toVector)

  extension (bits: Bits)

    def mostCommon: Char =
      if bits.count(_ == Zero) > bits.count(_ == One) then Zero else One

    def leastCommon: Char =
      if bits.count(_ == Zero) > bits.count(_ == One) then One else Zero

    def toInt: Int =
      Integer.parseInt(bits.mkString, 2)



  def rating(bits: Vector[Bits], commonOf: Bits => Char): Int =
    @tailrec
    def filter(todo: Vector[Bits], idx: Int = 0): Bits =
      if todo.size == 1 then
        todo.head
      else
        val common = commonOf(todo.transpose.apply(idx))
        val next   = todo.filter(bit => bit(idx) == common)
        filter(next, idx + 1)
    filter(bits).toInt


  lazy val gamma: Int            = diagnostics.transpose.map(_.mostCommon).toInt
  lazy val epsilon: Int          = diagnostics.transpose.map(_.leastCommon).toInt
  override lazy val answer1: Int = gamma * epsilon

  lazy val oxygenRating: Int       = rating(diagnostics, _.mostCommon)
  lazy val co2SchrubberRating: Int = rating(diagnostics, _.leastCommon)
  override lazy val answer2: Int   = oxygenRating * co2SchrubberRating
