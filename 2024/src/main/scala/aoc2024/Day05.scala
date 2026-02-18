package aoc2024

import nmcb.AoC

import scala.annotation.tailrec

object Day05 extends AoC:

  case class Rule(before: Int, after: Int):

    def applicable(x: Int, y: Int): Boolean =
      (before == x && after == y) || (before == y && after == x)

  case class Update(order: Vector[Int]):

    private val unvalidated: Map[Int, Vector[Int]] =
      @tailrec
      def loop(todo: Vector[Int], result: Map[Int, Vector[Int]] = Map.empty[Int, Vector[Int]]): Map[Int, Vector[Int]] =
        todo.runtimeChecked match
          case Vector()  => result
          case p +: rest => loop(rest, result + (p -> rest))
      loop(order)

    def validBy(rules: Vector[Rule]): Boolean =
      rules.forall: rule =>
        unvalidated.forall: (first, rest) =>
          rest.forall: page =>
            !(rule.before == page && rule.after == first)

    def middle: Int =
      order(order.length / 2)

    def sorted(using ordering: Ordering[Int]): Update =
      Update(order.sorted)

  val rules: Vector[Rule] = chunks(0).map:
    case s"$b|$a" => Rule(b.toInt, a.toInt)

  val updates: Vector[Update] = chunks(1).map: line =>
    Update(line.split(',').map(_.toInt).toVector)

  given ruleBasedOrdering: Ordering[Int] =
    (x: Int, y: Int) =>
      rules.find(_.applicable(x, y)) match
        case None    => 0
        case Some(r) => if r.before == x && r.after == y then 1 else -1

  override lazy val answer1: Int = updates.filter(_.validBy(rules)).map(_.middle).sum
  override lazy val answer2: Int = updates.filterNot(_.validBy(rules)).map(_.sorted).map(_.middle).sum
