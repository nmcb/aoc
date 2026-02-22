package aoc2022

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec
import scala.util.*

object Day11 extends AoC:

  lazy val monkeys: Vector[Monkey] =
    chunks.map(Monkey.fromStrings)

  object Monkey:
    def fromStrings(ss: Vector[String]): Monkey =

      def parseMonkey(s: String): Int =
        s match
          case s"Monkey $nr:" => nr.toInt

      def parseItems(s: String): Vector[Long] =
        s match
          case s"  Starting items: $is" => is.split(',').map(n => n.trim.toLong).toVector

      def parseOperation(s: String): (String, String) =
        s match
          case s"  Operation: new = old $op $r" => (op, r)

      def parseTest(s: String): Long =
        s match
          case s"  Test: divisible by $d" => d.toLong

      def parseToIfTrue(s: String): Int =
        s match
          case s"    If true: throw to monkey $m" => m.toInt

      def parseToIfFalse(s: String): Int =
        s match
          case s"    If false: throw to monkey $m" => m.toInt

      val nr        = parseMonkey(ss(0))
      val items     = parseItems(ss(1))
      val (op,rhs)  = parseOperation(ss(2))
      val divisible = parseTest(ss(3))
      val tit       = parseToIfTrue(ss(4))
      val tif       = parseToIfFalse(ss(5))

      Monkey(nr, items, op, Try(rhs.toLong).toOption, divisible, tit, tif)


  case class Monkey(nr: Int
                    , items: Vector[Long]
                    , operation: String
                    , rhs: Option[Long]
                    , divisible: Long
                    , throwToIfTrue: Int
                    , throwToIfFalse: Int
                    , lowerWorryBy: Long => Long = identity
                    , worries: Long = 0
  ):

    def operation(i: Long): Long =
      operation match
        case "+" => i + rhs.getOrElse(i)
        case "*" => i * rhs.getOrElse(i)

    private def inspectPackage(currentWorry: Long): (Long, Int) =
      val worry: Long  = (operation andThen lowerWorryBy)(currentWorry)
      val throwTo: Int = if worry % divisible == 0 then throwToIfTrue else throwToIfFalse
      (worry, throwTo)

    def inspectPackages: Vector[(Long, Int)] =
      items.map(inspectPackage)

  extension (monkeys: Vector[Monkey])

    def withLowerWorryBy(f: Long => Long): Vector[Monkey] =
      monkeys.map(_.copy(lowerWorryBy = f))

    def nextRound: Vector[Monkey] =
      @tailrec
      def loop(todo: Vector[Monkey], result: Vector[Monkey]): Vector[Monkey] =
        todo.runtimeChecked match
          case Vector() => result
          case monkey +: rest =>
            val inspections = result(monkey.nr).inspectPackages
            val throws  = inspections.foldLeft(result):
              case (acc, (worry, throwTo)) => acc.updated(throwTo, acc(throwTo).copy(items = acc(throwTo).items :+ worry))
            val next = throws.updated(monkey.nr, throws(monkey.nr).copy(items = Vector.empty, worries = monkey.worries + inspections.size))
            loop(rest, next)
      loop(monkeys, monkeys)

  def solve(rounds: Int, monkeys: Vector[Monkey]): Long =
    Iterator
      .iterate(monkeys)(_.nextRound)
      .nth(rounds)
      .map(_.worries)
      .sorted
      .takeRight(2)
      .product


  override lazy val answer1: Long =
    val denominator = 3
    val monkeys1    = monkeys.withLowerWorryBy(_ / denominator)
    solve(20, monkeys1)

  override lazy val answer2: Long =
    val denominator = monkeys.map(_.divisible).product
    val monkeys2    = monkeys.withLowerWorryBy(_ % denominator)
    solve(10000, monkeys2)
