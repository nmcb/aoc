package aoc2017

import nmcb.*
import scala.annotation.tailrec

object Day10 extends AoC:

  case class KnotHash(sparseHash: Vector[Int], current: Int, skipSize: Int):

    private inline def indexPlus(addition: Int) =
      (current + addition) % sparseHash.size

    private infix def reverse(length: Int): KnotHash =
      val buffer = sparseHash.toBuffer
      for
        i <- 0 until length
      do
        val swap = indexPlus(i)
        val by   = indexPlus(length - i - 1)
        buffer(swap) = sparseHash(by)

      val nextSparseHash = buffer.toVector
      val nextCurrent    = indexPlus(length + skipSize)
      val nextSkipSize   = skipSize + 1
      KnotHash(nextSparseHash, nextCurrent, nextSkipSize)

    def productOfFirstTwo: Int =
      sparseHash.take(2).product

    def denseHash: Vector[Int] =
      sparseHash.grouped(16).map(_.reduce(_ ^ _)).toVector

    override def toString: String =
      denseHash.map(_.toHexString).mkString("")

  object KnotHash:

    def init: KnotHash =
      KnotHash(Vector.range(0, 256), 0, 0)

    def compute(lengths: Vector[Int], runs: Int): KnotHash =
      @tailrec
      def loop(todo: Int, hash: KnotHash = init): KnotHash =
        if todo <= 0 then
          hash
        else
          loop(todo - 1, lengths.foldLeft(hash)(_ reverse _))
      loop(todo = runs)

    def compute(input: String, runs: Int): KnotHash =
      val salted = input.map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)
      compute(salted, runs)

  val lengths: Vector[Int] =
    input
      .split(",")
      .map(_.toInt)
      .toVector

  lazy val answer1: Int    = KnotHash.compute(lengths, runs = 1).productOfFirstTwo
  lazy val answer2: String = KnotHash.compute(input, runs = 64).toString
