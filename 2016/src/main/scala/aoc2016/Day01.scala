package aoc2016

import nmcb.*
import nmcb.pos.*
import scala.annotation.tailrec
import scala.util.Try

object Day01 extends AoC:

  case class Cmd(turn: Char, dist: Int)

  object Cmd:
    def fromString(s: String): Cmd =
      Try(Cmd(s.trim.head, s.trim.tail.toInt))
        .getOrElse(sys.error(s"unable to parse: $s"))

  case class Me(x: Int, y: Int, dir: Dir):

    def manhattan: Int =
      x.abs + y.abs

    infix def process(cmd: Cmd): Vector[Me] =
      (dir, cmd.turn).runtimeChecked match
        case (N, 'L') => (1 to cmd.dist).map(dist => copy(x = x - dist, dir = W)).toVector
        case (E, 'L') => (1 to cmd.dist).map(dist => copy(y = y + dist, dir = N)).toVector
        case (S, 'L') => (1 to cmd.dist).map(dist => copy(x = x + dist, dir = E)).toVector
        case (W, 'L') => (1 to cmd.dist).map(dist => copy(y = y - dist, dir = S)).toVector
        case (N, 'R') => (1 to cmd.dist).map(dist => copy(x = x + dist, dir = E)).toVector
        case (E, 'R') => (1 to cmd.dist).map(dist => copy(y = y - dist, dir = S)).toVector
        case (S, 'R') => (1 to cmd.dist).map(dist => copy(x = x - dist, dir = W)).toVector
        case (W, 'R') => (1 to cmd.dist).map(dist => copy(y = y + dist, dir = N)).toVector

  object Me:

    def airDrop: Me =
      Me(0, 0, N)

  val commands: Vector[Cmd] = input.mkString.split(',').map(Cmd.fromString).toVector

  @tailrec
  def solve(commands: Vector[Cmd], path: Vector[Me]): Me =

    @tailrec
    def twice(test: Vector[Me], visited: Vector[Me]): Option[Me] =
      test.runtimeChecked match
        case Vector()                                                   => None
        case h +: t if visited.exists(me => me.x == h.x && me.y == h.y) => Some(h)
        case _ +: t                                                     => twice(t, visited)

    val next = path.last.process(commands.head)
    if twice(next, path).nonEmpty then twice(next, path).get else solve(commands.tail :+ commands.head, path :++ next)

  override lazy val answer1: Int = commands.foldLeft(Vector(Me.airDrop))(_.last process _).last.manhattan
  override lazy val answer2: Int = solve(commands, Vector(Me.airDrop)).manhattan
