package aoc2016

import nmcb.*
import nmcb.predef.*
import nmcb.pos.{*, given}

import java.security.MessageDigest
import scala.collection.*

object Day17 extends AoC:

  val md5: MessageDigest = MessageDigest.getInstance("MD5")

  extension (p: Pos)

    def move(direction: Char): Pos =
      direction match
        case 'U' => (x = p.x, y = p.y - 1)
        case 'D' => (x = p.x, y = p.y + 1)
        case 'L' => (x = p.x - 1, y = p.y)
        case 'R' => (x = p.x + 1, y = p.y)


  /** hardcoded for 4x4 grid */
  case class Path(passcode: String, path: String = "", target: Pos = Pos.of(0, 0)):

    def withinGrid: Boolean =
      target.x >= 0 && target.x < 4 && target.y >= 0 && target.y < 4

    def reachedVault: Boolean =
      target == Pos.of(3, 3)

    def openDoors: String =
      md5
        .digest((passcode + path)
        .getBytes)
        .toHexString
        .take(4)
        .zip("UDLR")
        .foldLeft(""):
          case (result, (char, direction)) =>
            if "bcdef".contains(char) then result + direction else result

    def candidates: Vector[Path] =
      openDoors
        .foldLeft(Vector.empty[Path]): (result, direction) =>
          result :+ copy(path = path + direction, target = target.move(direction))
        .filter(_.withinGrid)

    /** breadth first search - return first reached */
    def solve1: Option[Path] =
      val todo    = mutable.Queue(this)
      var current = todo.dequeue

      while !current.reachedVault do
        current.candidates.foreach(todo.enqueue)
        if todo.isEmpty then return None
        current = todo.dequeue

      Some(current)

    /** breadth first search - return last reached */
    def solve2: Option[Path] =
      val todo   = mutable.Queue(this)
      var result = Option.empty[Path]

      while todo.nonEmpty do
        val current = todo.dequeue
        if current.reachedVault then
          result = Some(current)
        else
          current.candidates.foreach(todo.enqueue)

      result

  override lazy val answer1: String = Path(passcode = "vwbaicqe").solve1.map(_.path).getOrElse("<none>")
  override lazy val answer2: Int    = Path(passcode = "vwbaicqe").solve2.map(_.path.length).getOrElse(-1)
