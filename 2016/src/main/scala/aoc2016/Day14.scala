package aoc2016

import nmcb.*
import nmcb.predef.*

import java.security.MessageDigest
import scala.util.matching.UnanchoredRegex

object Day14 extends AoC:

  val md5: MessageDigest = MessageDigest.getInstance("MD5")

  val Three: UnanchoredRegex = "(.)\\1{2}".r.unanchored
  val Five: UnanchoredRegex  = "(.)\\1{4}".r.unanchored

  def hash(string: String): String =
    md5.digest(string.getBytes).toHexString

  def solve(hash: Int => String): Int =
    def check(window: Seq[(String, Int)]): Boolean =
      window.head match
        case (Three(t), _) =>
          window.tail.exists:
            case (Five(f), _) if f == t => true
            case _                      => false
        case _                          => false

    val (_, index) = Iterator.from(0).map(hash).zipWithIndex.sliding(1 + 1000).filter(check).nth(63).head
    index

  val puzzle = "zpqevtbw"

  def stretched(string: String): String =
    Iterator.iterate(string)(hash).nth(2016 + 1)

  lazy val answer1: Int = solve(index => hash(s"$puzzle$index"))
  lazy val answer2: Int = solve(index => stretched(s"$puzzle$index"))
