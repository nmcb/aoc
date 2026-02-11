package aoc2016

import nmcb.*

import scala.CanEqual.derived
import scala.collection.mutable

object Day10 extends AoC:

  enum Target(val index: Int) derives CanEqual:
    case Bot(override val index: Int) extends Target(index)
    case Out(override val index: Int) extends Target(index)

  import Target.*
  
  enum Inst:
    case Init(bot: Bot, value: Int)
    case Sort(bot: Bot, low: Target, high: Target)

  import Inst.*

  type TargetValues[A] = (Target, Set[A])

  extension [A](tvs: TargetValues[A])
    def target: Target = tvs._1
    def values: Set[A] = tvs._2

  private def solve(instructions: Vector[Inst]) =

    val targets: mutable.Map[Target,Set[Int]] =
      mutable.Map[Target,Set[Int]]().withDefaultValue(Set.empty)

    instructions.foreach:
      case Init(bot, value) => targets += bot -> (targets(bot) + value)
      case _                =>

    val sorts: mutable.Map[Target,Set[Set[Int]]] =
      mutable.Map[Target,Set[Set[Int]]]().withDefaultValue(Set.empty)

    var changed: Boolean = true

    while changed do
      changed = false
      targets.find(_.values.size == 2) match
        case Some((target, values)) =>
          instructions
            .find:
              case Sort(bot, _, _) => bot == target
              case _               => false
            match
              case Some(Sort(bot, low, high)) =>
                targets += low  -> (targets(low) + values.min)
                targets += high -> (targets(high) + values.max)
                targets += bot  -> Set.empty
                sorts   += bot  -> (sorts(bot) + values)
                changed = true
              case _ =>
        case None =>

    (targets, sorts)

  def solve1(instructions: Vector[Inst], values: Set[Int]): Int =
    val (_, targets) = solve(instructions)
    targets.find(_.values.contains(values)).get.target.index

  def solve2(instructions: Vector[Inst]): Int =
    val (targets, _) = solve(instructions)
    targets(Out(0)).head * targets(Out(1)).head * targets(Out(2)).head

  private val ParseInit =
    """value (\d+) goes to bot (\d+)""".r

  private val ParseSort =
    """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r

  def targetFromString(s: String, i: Int): Target =
    s match
      case "bot" => Bot(i)
      case "output" => Out(i)

  def instFromString(s: String): Inst =
    s match
      case ParseInit(value, index) =>
        Init(Bot(index.toInt), value.toInt)
      case ParseSort(index, lowTarget, low, highTarget, high) =>
        Sort(Bot(index.toInt), targetFromString(lowTarget, low.toInt), targetFromString(highTarget, high.toInt))

  def instructionsFromString(s: String): Vector[Inst] =
    s.linesIterator.map(instFromString).toVector

  def solvePart1(input: String, values: Set[Int] = Set(61, 17)): Int =
    solve1(instructionsFromString(input), values)

  def solvePart2(input: String): Int =
    solve2(instructionsFromString(input))

  override lazy val answer1: Int = solvePart1(input)
  override lazy val answer2: Int = solvePart2(input)
