package examples

import java.lang.System.currentTimeMillis
import scala.util.Try

object VampireNumber:

  extension (digits: String)
    def possibleFangs(length: Int): Iterator[String] =
      digits.combinations(length).flatMap(_.permutations).distinct

  def isVampireNumber(number: Int): Boolean =
    val digits: String = number.toString
    if digits.length % 2 != 0 then
      false
    else
      val fangWidth = digits.length / 2
      digits.possibleFangs(fangWidth).exists: leftDigits =>
        val leftFang = leftDigits.toInt
        if leftFang != 0 then
          val rightFang = number / leftFang
          if leftFang % 10 == 0 && rightFang % 10 == 0 then
            false
          else
            if rightFang * leftFang == number then
              val rightDigits = rightFang.toString
              digits.sorted == (leftDigits + rightDigits).sorted
            else false
        else
          false

  def sieve(max: Int): Seq[Int] =
    (0 to max).filter(isVampireNumber)


  def main(args: Array[String]): Unit =
    val max: Int         = Try(args(0).toInt).getOrElse(1000000)
    val start: Long      = currentTimeMillis
    val result: Seq[Int] = sieve(max)

    println(s"vampire numbers up to $max:")
    println(result.mkString(", "))
    println(s"took ${currentTimeMillis - start} ms")
