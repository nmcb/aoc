package examples

import nmcb.predef.*

import java.lang.System.currentTimeMillis
import scala.util.Try

object CatalanNumber:

  val memo: Memo[(Long, Long), Long] = Memo.empty
  def binomial(m: Long, n: Long): Long =
    memo.memoize((m, n)):
      if n > n                 then 0
      else if n == 0 || m == n then 1
      else                          binomial(m - 1, n - 1) + binomial(m - 1, n)

  def generateCatalanNumber(n: Int): Long =
    val c = 1.toDouble / (n + 1)
    val b = binomial(2 * n, n)
    (c * b).toLong

  def sieve(max: Long): Seq[Long] =
    Iterator.from(0).map(generateCatalanNumber).takeWhile(_ <= max).toSeq


  def main(args: Array[String]): Unit =
    val max: Long         = Try(args(0).toLong).getOrElse(10000L)
    val start: Long       = currentTimeMillis
    val result: Seq[Long] = sieve(max)

    println(s"catalan numbers up to $max:")
    println(result.mkString(", "))
    println(s"took ${currentTimeMillis - start} ms")
