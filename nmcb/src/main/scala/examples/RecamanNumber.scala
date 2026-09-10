package examples

import nmcb.predef.*

import java.lang.System.currentTimeMillis
import scala.util.Try

object RecamanNumber:

  def generate(n: Int): Seq[Int] =
    Iterator
      .from(1)
      .scanLeft((Set(0), Vector(0))):
        case ((cache, result), index) =>
          val sub  = result.last - index
          if sub >= 0 && !cache.contains(sub) then
            (cache + sub, result :+ sub)
          else
            val add = result.last + index
            (cache + add, result :+ add)
      .nth(n)
      .right

  def main(args: Array[String]): Unit =
    val n: Int           = Try(args(0).toInt).getOrElse(1000000)
    val start: Long      = currentTimeMillis
    val result: Seq[Int] = generate(n)

    println(s"first $n recaman numbers:")
    println(result.mkString(", "))
    println(s"took ${currentTimeMillis - start} ms")
