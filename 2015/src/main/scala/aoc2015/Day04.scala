package aoc2015

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day04 extends AoC:

  val secret: String = "yzbqklnj"

  def hashMD5(s: String): String =
    import java.security.MessageDigest
    MessageDigest.getInstance("MD5").digest(s.getBytes).toHexString
  
  def solve(prefix: String): Int =
    @tailrec
    def loop(i: Int = 1): Int = if hashMD5(secret + i.toString).startsWith(prefix) then i else loop(i + 1)
    loop()

  lazy val answer1: Int = solve("00000")
  lazy val answer2: Int = solve("000000")
