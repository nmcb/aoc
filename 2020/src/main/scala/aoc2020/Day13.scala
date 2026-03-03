package aoc2020

import nmcb.*

import scala.annotation.*
import scala.util.*

object Day13 extends AoC:

  val Vector(begin, puzzle) = lines

  val schedule: List[(Long,Int)] =
    puzzle
      .split(',')
      .zipWithIndex
      .foldLeft(List.empty):
        case (result,(bus,offset)) =>
          Try(bus.toLong).map(b => result ++ Map(b -> offset)).getOrElse(result)

  def depart(at: Long)(bus: Long): Boolean = at % bus == 0

  def solve1(schedule: List[(Long,Int)], start: Long): Long =
    @tailrec
    def go(time: Long): Long =
      schedule.toMap.keys.find(depart(time)) match
        case None      => go(time + 1)
        case Some(bus) => (time - start) * bus
    go(start)

  /**
   * Chinese Remainder Theorem, allows you to solve equations of type:
   *
   * x % n[0] == a[0]
   * x % n[1] == a[1]
   * x % n[2] == a[2]
   * ...
   *
   * @return x
   */
  def crt(n: List[Long], a: List[Long]): Option[Long] =

    require(n.size == a.size)
    val prod = n.product

    @tailrec
    def loop(n: List[Long], a: List[Long], sm: Long): Long =

      def gcd(a: Long, b: Long): Long =
        @tailrec
        def loop(a: Long, b: Long, nominator: Long, denominator: Long): Long =
          if a > 1 then loop(b, a % b, denominator - (a / b) * nominator, nominator) else denominator

        if b == 1 then 1 else
          val result = loop(a, b, 0, 1)
          if result < 0 then result + b else result

      if n.isEmpty then sm else
        val p = prod / n.head
        loop(n.tail, a.tail, sm + a.head * gcd(p, n.head) * p)
 
    Try(loop(n, a, 0) % prod) match
      case Success(v) => Some(v)
      case _          => None

  val equations: Map[Long,Long] =
    puzzle
      .split(',')
      .zip((0 until puzzle.length).map(_.toLong).reverse)
      .map((b,i) => (Try(b.toLong).getOrElse[Long](1),i))
      .toMap

  override lazy val answer1: Long = solve1(schedule, begin.toLong)
  override lazy val answer2: Long = crt(equations.keys.toList, equations.values.toList).getOrElse(sys.error("boom")) - puzzle.length + 1
