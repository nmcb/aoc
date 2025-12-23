package aoc2019

import nmcb.*

object Day22 extends AoC:

  /**
   * I have no idea why this works...
   *
   * @see Credits - https://github.com/maneatingape
   */
  case class Technique(a: BigInt, c: BigInt, m: BigInt):
    def mod(n: BigInt): BigInt =
      n % m

    def shuffle(index: Long): Long =
      mod(a * index + c).toLong

    infix def merge(that: Technique): Technique =
      val nextA = mod(a * that.a)
      val nextC = mod(c * that.a + that.c)
      copy(a = nextA, c = nextC)

    def inverse: Technique =
      val nextA = a.modInverse(m)
      val nextC = mod(nextA * -c)
      copy(a = nextA, c = nextC)

    def pow(exp: BigInt): Technique =
      val nextA = a.modPow(exp, m)
      val nextC = mod((nextA - 1) * (a - 1).modInverse(m) * c)
      copy(a = nextA, c = nextC)

  def solve(input: Seq[String], size: Long): Technique =
    input.map(_.split(" "))
      .map:
        case Array(_, "into", _, _) => Technique(size - 1, size - 1, size)
        case Array(_, "with", _, n) => Technique(n.toLong, 0, size)
        case Array("cut", n)        => Technique(1, size - n.toLong, size)
      .reduce(_ merge _)

  lazy val answer1: Long = solve(lines, 10007).shuffle(2019)
  lazy val answer2: Long = solve(lines, 119315717514047L).inverse.pow(101741582076661L).shuffle(2020)
