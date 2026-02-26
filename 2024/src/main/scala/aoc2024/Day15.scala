package aoc2024

import nmcb.*
import nmcb.pos.*

import scala.annotation.*

object Day15 extends AoC:

  type Command = Char

  extension (p: Pos)

    def move(d: Command): Pos =
      d match
        case '^' => (x = p.x, y = p.y - 1)
        case '>' => (x = p.x + 1, y = p.y)
        case 'v' => (x = p.x, y = p.y + 1)
        case '<' => (x = p.x - 1, y = p.y)

    def coordinate: Int =
      100 * p.y + p.x

  type Move = (Pos,Pos)
  
  extension (move: Move)
    def from: Pos = move._1
    def to: Pos   = move._2

  case class Grid(grid: Map[Pos, Char]):
    lazy val sizeX: Int = grid.keys.maxBy(_.x).x + 1
    lazy val sizeY: Int = grid.keys.maxBy(_.y).y + 1
    lazy val robotPosition: Pos     = grid.find((_, c) => c == '@').map(_.pos).head
    lazy val boxPositions: Set[Pos] = grid.filter((_, c) => c == 'O' | c == '[').keySet

    def isFree(p: Pos): Boolean = grid(p) == '.'
    def isWall(p: Pos): Boolean = grid(p) == '#'
    def isBox(p: Pos): Boolean  = grid(p) == 'O' | grid(p) == ']' | grid(p) == '['

    def moves(p: Pos, d: Command): Set[Move] =
      @tailrec
      def loop(todo: Set[Pos], updates: Set[Move] = Set.empty): Set[Move] =

        def widen(c: Set[Pos]): Set[Pos] =
          val pl = c.minBy(_.x)
          val pr = c.maxBy(_.x)
          val l = if (d == '^' | d == 'v') && grid(pl) == ']' then Set((x = pl.x - 1, y = pl.y)) else Set.empty
          val r = if (d == '^' | d == 'v') && grid(pr) == '[' then Set((x = pr.x + 1, y = pr.y)) else Set.empty
          l ++ c ++ r

        def unblocked(ms: Set[Pos]): Boolean = ms.forall(isFree)
        def blocked(ms: Set[Pos]): Boolean   = ms.exists(isWall)

        val moves = widen(todo).map(from => from -> from.move(d))
        val tos   = moves.map(_.to)

        if      unblocked(tos) then updates ++ moves
        else if blocked(tos)   then Set.empty
        else                        loop(todo = widen(tos).filter(isBox), updates = updates ++ moves)

      loop(Set(p))

    infix def push(d: Command): Grid =
      val updates = moves(robotPosition, d)
      if updates.isEmpty then
        this
      else
        val updated: Map[Pos,Char] =
          updates.foldLeft(grid):
            case (g,(f,t)) => g.updated(t, grid(f))

        val updatedAndCleaned: Map[Pos,Char] =
          val froms = updates.map(_.from)
          val tos   = updates.map(_.to)
          froms.diff(tos).foldLeft(updated)((g, p) => g.updated(p, '.'))

        Grid(updatedAndCleaned)

    def resize: Grid =

      def map(p: Pos, from: Char, to: Char) =
        Map((x = p.x * 2, y = p.y) -> from, (x = (p.x * 2) + 1, y = p.y) -> to)

      Grid(
        grid.flatMap:
          case (p,'#') => map(p, '#', '#')
          case (p,'.') => map(p, '.', '.')
          case (p,'O') => map(p, '[', ']')
          case (p,'@') => map(p, '@', '.')
          case (p, c ) => sys.error(s"invalid element: p=$p, c=$c")
      )

  val (grid: Grid, moves: List[Command]) =
    val Array(top, bottom) = input.split("\n\n").map(_.trim)
    val positions = for {
      (l, y) <- top.split("\n").zipWithIndex
      (c, x) <- l.trim.zipWithIndex
    } yield (x, y) -> c
    val moves = bottom.filterNot(_ == '\n').toList
    (Grid(positions.toMap), moves)

  override lazy val answer1: Int = moves.foldLeft(grid)(_ push _).boxPositions.map(_.coordinate).sum
  override lazy val answer2: Int = moves.foldLeft(grid.resize)(_ push _).boxPositions.map(_.coordinate).sum
