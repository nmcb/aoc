package examples

import java.lang.System.currentTimeMillis
import scala.util.Try

object VampireNumber:

  extension (digits: String)
    def possibleFangs(length: Int): Iterator[String] =
      digits.combinations(length).flatMap(_.permutations).distinct

  def isVampireNumber(n: Int): Boolean =
    val digits: String = n.toString
    if digits.length % 2 != 0 then
      false
    else
      val fangLength = digits.length / 2
      digits.possibleFangs(fangLength).exists: left =>
        digits.diff(left).possibleFangs(fangLength).exists: right =>
          n == left.toInt * right.toInt

  def main(args: Array[String]): Unit =
    val max: Int         = Try(args(0).toInt).getOrElse(1000000)
    val start: Long      = currentTimeMillis
    val result: Seq[Int] = (0 to max).filter(isVampireNumber)

    println(s"vampire numbers up to $max:")
    println(result.mkString(", "))
    println(s"took ${currentTimeMillis - start} ms")
