package aoc2017

import nmcb.*
import nmcb.predef.*

object Day04 extends AoC:

  val memo: Memo[String, Map[Char, Int]] = Memo.empty
  extension (self: String)
    def charCount: Map[Char, Int] =
      memo.memoize(self):
        self.groupMapReduce(identity)(_ => 1)(_ + _)

  extension (l: String) infix def anagram(r: String): Boolean =
    l.charCount == r.charCount

  case class Passphrase(words: Vector[String]):

    def valid1: Boolean =
      words.size == words.distinct.size

    def valid2: Boolean =
      words.combinations(2).forall(ws => !(ws.head anagram ws.last))

  val passphrases: Vector[Passphrase] =
    lines
      .map(_.trim.split("\\s+").toVector)
      .map(Passphrase.apply)

  override lazy val answer1: Int = passphrases.count(_.valid1)
  override lazy val answer2: Int = passphrases.count(_.valid2)
