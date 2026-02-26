package aoc2023

import nmcb.*
import nmcb.pos.{*, given}
import nmcb.predef.*

import scala.annotation.*

object Day03 extends AoC:
  

  case class Page(chars: Grid[Char]):

    extension (char: Char)
      
      def isGear: Boolean =
        char == '*'
      
      def isSymbol: Boolean =
        char != '.' && !char.isDigit

    extension (pos: Pos)
      
      def isDigit: Boolean =
        chars.peek(pos).isDigit
  
      def isSymbol: Boolean =
        chars.peek(pos).isSymbol
  
      def isGear: Boolean =
        chars.peek(pos).isGear

    def adjacent(pos: Pos): Set[Pos] =
      Pos.offset8
        .map(d => pos + d)
        .filter(chars.within)

    case class Num(loc: Vector[Pos], value: Int, symbols: Map[Pos, Char])

    object Num:
      
      def apply(loc: Vector[Pos]): Num =
        val number  = loc.map(p => chars.peek(p)).mkString("").toInt
        val symbols = loc.flatMap(adjacent).filter(_.isSymbol).map(p => p -> chars.peek(p)).toMap
        Num(loc, number, symbols)

    lazy val numbers: Vector[Num] =
      @tailrec
      def compute(pos: Pos = (0,0), cur: String = "", loc: Vector[Pos] = Vector.empty, acc: Vector[Num] = Vector.empty): Vector[Num] =
        if pos.y >= chars.sizeY then
          acc
        else
          if pos.x >= chars.sizeX then
            compute((0, pos.y + 1), "", Vector.empty, if cur.nonEmpty then acc :+ Num(loc) else acc)
          else
            if pos.isDigit then
              compute((pos.x + 1, pos.y), s"$cur${chars.peek(pos)}", loc :+ pos, acc)
            else
              compute((pos.x + 1, pos.y), "", Vector.empty, if cur.nonEmpty then acc :+ Num(loc) else acc)
      compute()

    lazy val numbersWithAdjacentSymbols: Vector[Num] =
      numbers.filter(_.symbols.nonEmpty)

    lazy val gearsWithAdjacentNumbers: Map[Pos, Set[Num]] =
      chars
        .filter(_.right.isGear)
        .map(g => g.left -> numbersWithAdjacentSymbols.filter(_.symbols.exists(_.left == g.left)).toSet)
        .toMap

    lazy val gearRatios: Vector[Set[Num]] =
      val ratios =
        for
          (g1, ns1) <- gearsWithAdjacentNumbers
          (g2, ns2) <- gearsWithAdjacentNumbers
          if g1 == g2 && ns1.size == 2 && ns2.size == 2 && (ns1 diff ns2).isEmpty
        yield
          ns1
      ratios.toVector

  val page: Page = Page(chars = Grid.fromLines(lines))


  override lazy val answer1: Int = page.numbersWithAdjacentSymbols.map(_.value).sum
  override lazy val answer2: Int = page.gearRatios.map(_.map(_.value).product).sum
