package aoc2020

import nmcb.*

import scala.util.matching.Regex

object Day02 extends AoC:

  type Predicate = Int => Int => Char => String => Boolean

  val Line: Regex = "(\\d+)-(\\d+)\\s(.):\\s(.+)".r

  def solve(predicate: Predicate): Int = lines.count:
    case s"$x-$y $char: $password" => predicate(x.toInt)(y.toInt)(char.head)(password)

  // check that given min and max number of given char are in given passwd
  def check1: Predicate = min => max => char => passwd =>
    val count = passwd.count(_ == char)
    count >= min && count <= max

  // check that exactly one of given (one-based) indices in given passwd contains given char
  def check2: Predicate = x => y => char => passwd =>
    val chars = passwd.zipWithIndex.map(_.swap).toMap
    chars.get(x - 1).contains(char) ^ chars.get(y - 1).contains(char)


  override lazy val answer1: Int = solve(check1)
  override lazy val answer2: Int = solve(check2)
