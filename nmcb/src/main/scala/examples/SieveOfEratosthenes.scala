package examples

import scala.collection.mutable.ArrayBuffer

object SieveOfEratosthenes:

  def sieve(n: Int): Seq[Int] =
    val primes = ArrayBuffer.fill(n + 1)(true)
    for
      i <- 2 to Math.sqrt(n).toInt
      if primes(i)
    do
      for
        j <- (i * i) to n by i
      do
        primes(j) = false

    for
      m <- primes.indices.drop(2)
      if primes(m)
    yield
      m

  def main(args: Array[String]): Unit =
    val max: Int = 100
    val result: Seq[Int] = sieve(max)
    println(s"primes up to $max: $result")

