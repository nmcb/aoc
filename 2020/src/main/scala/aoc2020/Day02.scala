package aoc2020

import nmcb.*

import scala.util.matching.Regex

object Day02 extends AoC:

  type Predicate = Int => Int => Char => String => Boolean

  val Line: Regex = "(\\d+)-(\\d+)\\s(.):\\s(.+)".r

  def answer(predicate: Predicate): Int = lines.count:
    case s"$x-$y $char: $password" => predicate(x.toInt)(y.toInt)(char.head)(password)

  def check1: Predicate = min => max => char => passwd =>
    val count = passwd.count(_ == char)
    count >= min && count <= max
    
  def check2: Predicate = x => y => char => passwd =>
    val chars = passwd.zipWithIndex.map(_.swap).toMap
    chars.get(x - 1).contains(char) ^ chars.get(y - 1).contains(char)


  override lazy val answer1: Int = answer(check1)
  override lazy val answer2: Int = answer(check2)
