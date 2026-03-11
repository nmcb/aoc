package aoc2016

import nmcb.*
import nmcb.pos.{*, given}

import scala.annotation.tailrec
import scala.util.Try

object Day01 extends AoC:

  case class Command(turn: Char, dist: Int)

  object Command:
    def fromString(s: String): Command =
      Try(Command(s.trim.head, s.trim.tail.toInt))
        .getOrElse(sys.error(s"unable to parse: $s"))

  case class Me(pos: Pos, dir: Dir):

    infix def process(cmd: Command): Vector[Me] =
      (dir, cmd.turn).runtimeChecked match
        case (N, 'L') | (S, 'R') => (1 to cmd.dist).map(dist => copy(pos.translate(dx = -dist), W)).toVector
        case (E, 'L') | (W, 'R') => (1 to cmd.dist).map(dist => copy(pos.translate(dy = +dist), N)).toVector
        case (S, 'L') | (N, 'R') => (1 to cmd.dist).map(dist => copy(pos.translate(dx = +dist), E)).toVector
        case (W, 'L') | (E, 'R') => (1 to cmd.dist).map(dist => copy(pos.translate(dy = -dist), S)).toVector

  object Me:
    val airDrop: Me = Me(pos = Pos.origin, dir = N)

  def solve1(commands: Vector[Command], start: Me): Long =
    commands.foldLeft(Vector(start))(_.last process _).last.pos.manhattanDistance(Pos.origin)

  def solve2(commands: Vector[Command], start: Me): Long =
    @tailrec
    def solve(commands: Vector[Command], path: Vector[Me]): Me =
      @tailrec
      def twice(test: Vector[Me], visited: Vector[Me]): Option[Me] =
        test.runtimeChecked match
          case Vector()                                        => None
          case h +: t if visited.exists(me => me.pos == h.pos) => Some(h)
          case _ +: t                                          => twice(t, visited)

      val h +: t = commands: @unchecked
      val next   = path.last.process(h)
      if twice(next, path).nonEmpty then twice(next, path).get else solve(t :+ h, path :++ next)

    solve(commands, Vector(start)).pos.manhattanDistance(Pos.origin)


  val commands: Vector[Command] = input.mkString.split(',').map(Command.fromString).toVector

  override lazy val answer1: Long = solve1(commands, Me.airDrop)
  override lazy val answer2: Long = solve2(commands, Me.airDrop)
