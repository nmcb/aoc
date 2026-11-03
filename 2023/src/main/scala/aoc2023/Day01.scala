package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day01 extends AoC:

  val digits: Map[String, Char] =
    (0 to 9).map(_.toString).map(n => n -> n.head).toMap

  def recover(replacements: Map[String, Char])(line: String): Int =

    @tailrec
    def firstLeftDigit(side: String): Option[(String, Char)] =
      if side.isEmpty then
        None
      else
        replacements.find((p, _) => side.startsWith(p)) match
          case Some(found) => Some(found)
          case None        => firstLeftDigit(side.tail)


    @tailrec
    def firstRightDigit(side: String): Option[(String, Char)] =
      if side.isEmpty then
        None
      else
        replacements.find((p, _) => side.endsWith(p)) match
          case Some(found) => Some(found)
          case None        => firstRightDigit(side.init)

    val (_, l) = firstLeftDigit(line).getOrElse(sys.error(s"no replacement found in: $line"))
    val (_, r) = firstRightDigit(line).getOrElse(sys.error(s"no replacement found in: $line"))
    s"$l$r".toInt

  val names: Map[String, Char] =
    Map(
      "one"   -> '1',
      "two"   -> '2',
      "three" -> '3',
      "four"  -> '4',
      "five"  -> '5',
      "six"   -> '6',
      "seven" -> '7',
      "eight" -> '8',
      "nine"  -> '9',
      "zero"  -> '0'
    )

  override lazy val answer1: Int = lines.map(recover(digits)).sum
  override lazy val answer2: Int = lines.map(recover(digits ++ names)).sum
