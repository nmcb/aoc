package aoc2021

import nmcb.*
import nmcb.predef.*

object Day14 extends AoC:

  type Rules              = Map[(Char, Char), Char]
  type MoleculePairCount  = Map[(Char, Char), Long]
  type MoleculeCount      = Map[Char, Long]

  val template: String =
    lines.head

  val rules: Rules =
    lines
      .collect:
        case s"$pair -> $insert" => (pair.charAt(0), pair.charAt(1)) -> insert.charAt(0)
      .toMap

  /** maintain a count for sequences of pairs as well as individual molecules */
  case class PolymerCount(rules: Rules, moleculePairCount: MoleculePairCount, moleculeCount: MoleculeCount):

    def step: PolymerCount =

      val nextMoleculePairCount: MoleculePairCount =
        moleculePairCount
          .toVector
          .flatMap: (pair, count) =>
            val char = rules(pair)
            Vector((pair.left,char) -> count, (char, pair.right) -> count)
          .groupMapReduce(_.left)(_.right)(_ + _)

      val nextMoleculeCount: MoleculeCount =
        moleculePairCount
          .foldLeft(moleculeCount): (result, count) =>
            val char = rules(count.left)
            result.updated(char, result(char) + count.right)
          .groupMapReduce(_.left)(_.right)(_ + _)

      copy(moleculePairCount = nextMoleculePairCount, moleculeCount = nextMoleculeCount)

  object PolymerCount:

    def make(rules: Rules, template: String): PolymerCount =
      val pairs  = template.zip(template.tail).groupMapReduce(identity)(_ => 1L)(_+_)
      val counts = template.groupMapReduce(identity)(_ => 1L)(_ + _)
      PolymerCount(rules, pairs, counts)

  def solve(polymerCount: PolymerCount, iterations: Int): Long =
    val counts = Iterator.iterate(polymerCount)(_.step).nth(iterations).moleculeCount
    counts.values.max - counts.values.min

  val polymerCount: PolymerCount = PolymerCount.make(rules, template)

  override lazy val answer1: Long = solve(polymerCount, 10)
  override lazy val answer2: Long = solve(polymerCount, 40)
