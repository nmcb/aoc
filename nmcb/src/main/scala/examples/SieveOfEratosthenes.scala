package examples

import scala.math.sqrt
import scala.util.Try

object SieveOfEratosthenes:

  def sieve(n: Int): Seq[Int] =
    val primes = Array.fill(n + 1)(true)
    for
      i <- 2 to sqrt(n).toInt
      if primes(i)
    do
      for
        j <- (i * i) to n by i
      do
        primes(j) = false

    for
      p <- primes.indices.drop(2)
      if primes(p)
    yield
      p

  def main(args: Array[String]): Unit =
    val max: Int         = Try(args(0).toInt).getOrElse(1000)
    val start: Long      = System.currentTimeMillis
    val result: Seq[Int] = sieve(max)

    println(s"primes up to $max:")
    println(result.mkString(", "))
    println(s"took ${System.currentTimeMillis - start} ms")
