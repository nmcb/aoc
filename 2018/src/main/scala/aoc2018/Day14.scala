package aoc2018

import nmcb.*
import nmcb.predef.*

object Day14 extends AoC:

  val puzzle = 635041

  case class Recipes(elf1: Int, elf2: Int, scores: Vector[Int]):

    def next: Recipes =
      val score1 = scores(elf1)
      val score2 = scores(elf2)
      val sum    = score1 + score2
      val next   = if sum >= 10 then scores :+ (sum / 10) :+ (sum % 10) else scores :+ sum
      val index1 = (elf1 + score1 + 1) % next.length
      val index2 = (elf2 + score2 + 1) % next.length
      Recipes(index1, index2, next)

  object Recipes:
    def init: Recipes = Recipes(elf1 = 0, elf2 = 1, scores = Vector(3, 7))

  def solve1(input: Int): Int =
    Iterator
      .iterate(Recipes.init)(_.next)
      .findFirst(_.scores.size >= input + 10)
      .scores
      .slice(input, input + 10)
      .mkString
      .toInt

  def solve2(input: Int): Int =

    val digits = input.toString.map(_.asDigit)

    type Search = (Recipes, Int)

    object Search:
      val NotFound = -1
      def init: Search = (Recipes.init, NotFound)

    extension (search: Search)
      def recipes: Recipes           = search._1
      def scores: Vector[Int]        = search._1.scores
      def nrOfScoresLeftOfInput: Int = search._2

      def next: Search =
        (recipes.next, scores.indexOfSlice(digits, scores.length - digits.length - 1))

    Iterator
      .iterate(Search.init)(_.next)
      .findFirst(_.nrOfScoresLeftOfInput != Search.NotFound)
      .nrOfScoresLeftOfInput

  override lazy val answer1: Int = solve1(puzzle)
  override lazy val answer2: Int = solve2(puzzle)
