package aoc2022

import nmcb.*
import nmcb.pos.*
import nmcb.predef.*

import scala.annotation.*
import scala.math.*
import scala.math.Integral.Implicits.*

object Day17 extends AoC:

  val moves: Vector[Move] = input.toVector

  val origin: Pos = Pos.of(0,0)

  type Move = Char
  val L = '<'
  val R = '>'
  val D = 'v'

  object Move:
    val pattern: LazyList[Move] = moves.to(LazyList) #::: pattern

  sealed abstract class Rock(val relative: List[Pos]):
    def withOrigin(o: Pos): List[Pos] = relative.map(p => Pos.of(p.x + o.x, p.y + o.y))

  case object Min   extends Rock(List(Pos.of(0,0), Pos.of(1,0), Pos.of(2,0), Pos.of(3,0)))
  case object Plus  extends Rock(List(Pos.of(1,0), Pos.of(0,1), Pos.of(1,1), Pos.of(2,1), Pos.of(1,2)))
  case object El    extends Rock(List(Pos.of(0,0), Pos.of(1,0), Pos.of(2,0), Pos.of(2,1), Pos.of(2,2)))
  case object Stack extends Rock(List(Pos.of(0,0), Pos.of(0,1), Pos.of(0,2), Pos.of(0,3)))
  case object Box   extends Rock(List(Pos.of(0,0), Pos.of(1,0), Pos.of(0,1), Pos.of(1,1)))

  object Rock:
    val sequence: LazyList[Rock] = LazyList(Min, Plus, El, Stack, Box) #::: sequence


  case class Chamber(rocks: LazyList[Rock], pattern: LazyList[Move], stopped: Set[Pos], height: Int):
    import Chamber.*

    def isWall(p: Pos): Boolean     = p.x < origin.x | p.x >= origin.x + width
    def isFloor(p: Pos): Boolean    = p.y < origin.y
    def isOccupied(p: Pos): Boolean = isWall(p) | isFloor(p) | stopped.contains(p)

    def next: Chamber =

      def appear: Pos =
        origin.translate(dx = 2, dy = height + 3)

      @tailrec
      def drop(pos: Pos, moves: LazyList[Move], trace: List[Move] = List.empty): (List[Move], Pos) =

        val move = moves.head
        val rest = moves.tail

        val moved = move match
          case L => pos.translate(dx = -1, dy =  0)
          case R => pos.translate(dx =  1, dy =  0)
          case D => pos.translate(dx =  0, dy = -1)

        if rocks.head.withOrigin(moved).forall(p => !isOccupied(p)) then
          if move == L || move == R then
            drop(moved, D #:: rest, move :: trace)
          else
            drop(moved, rest, move :: trace)
        else
          if move == L || move == R then
            drop(pos, D #:: rest, move :: trace)
          else
            (trace, pos)

      val (moves, pos) = drop(appear, pattern)

      val dropped: Set[Pos] =
        stopped ++ rocks.head.withOrigin(pos)

      val maxY: Int =
        dropped.map(_.y).max + 1

      val consumed: LazyList[Move] =
        pattern.drop(moves.filterNot(_ == D).size)

      Chamber(rocks = rocks.tail, pattern = consumed, stopped = dropped.filter(_.y >= maxY - slidingWindow), height = maxY)

    def cycleInvariant: Set[Pos] =
      stopped.map(_ - Pos.of(0, height - slidingWindow))


  object Chamber:
    val slidingWindow: Int = 100
    val width: Int         = 7

    def empty: Chamber =
      Chamber(Rock.sequence, Move.pattern, Set.empty, 0)

  def solve1(nr: Int): Int =
    Iterator.iterate(Chamber.empty)(_.next).nth(nr).height

  def solve2(nr: Long): Long =
    val cycle: Cycle[Chamber] =
      Cycle.find(Chamber.empty, _.next, _.cycleInvariant)

    val (cycleNr, tailNr) =
      (nr - cycle.stemSize) /% cycle.cycleSize

    val stemHeight: Long =
      cycle.cycleHead.height

    val cycleHeight: Long =
      cycle.cycleHeadRepeat.height - stemHeight

    val tailHeight: Long =
      Iterator.iterate(cycle.cycleHead)(_.next).nth(tailNr.toInt).height - stemHeight

    stemHeight + cycleNr * cycleHeight + tailHeight



  override lazy val answer1: Int  = solve1(nr = 2022)
  override lazy val answer2: Long = solve2(nr = 1000_000_000_000L)
