package aoc2016

import nmcb.*
import scala.annotation.tailrec

object Day16 extends AoC:

  @tailrec
  def dragon(s: String, length: Int): String =
    val result = s + '0' + s.reverse.map(c => if c == '0' then '1' else '0')
    if result.length >= length then result.take(length) else dragon(result, length)

  @tailrec
  def checksum(s: String): String =
    val result = s.grouped(2).map(p => if p(0) == p(1) then '1' else '0').mkString
    if result.length % 2 == 0 then checksum(result) else result

  lazy val answer1: String = checksum(dragon("10001001100000001", 272))
  lazy val answer2: String = checksum(dragon("10001001100000001", 35651584))
