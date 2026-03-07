package aoc2015

import nmcb.*
import nmcb.pos.*

object Day03 extends AoC:

  type Deliveries = Map[Pos, Int]

  extension (d: Deliveries) def add(pos: Pos): Deliveries =
    d.updatedWith(pos)(_.orElse(Some(0)).map(_ + 1))

  object Deliveries:

    def init: Deliveries =
      Map().add(Pos.origin)

  type Command = Char

  case class Area(start: Pos = Pos.origin, deliveries: Deliveries = Deliveries.init):

    private def moveToAndDeliver(loc: Pos): Area =
      copy(start = loc, deliveries = deliveries.add(loc))

    infix def next(command: Command): Area =
      command match
        case '>' => moveToAndDeliver((x = start.x + 1, y = start.y))
        case '<' => moveToAndDeliver((x = start.x - 1, y = start.y))
        case '^' => moveToAndDeliver((x = start.x, y = start.y + 1))
        case 'v' => moveToAndDeliver((x = start.x, y = start.y - 1))


  object Area:

    def init: Area =
      Area(start = Pos.origin, deliveries = Deliveries.init)

  def solve1(commands: Vector[Command]): Int =
    commands.foldLeft(Area.init)(_ next _).deliveries.values.size

  def solve2(commands: Vector[Command]): Int =
    val (robot, santa) = commands.zipWithIndex.foldLeft((Area.init, Area.init)):
      case ((robot, santa), (command, index)) =>
        if (index % 2 != 0) (robot next command, santa) else (robot, santa next command)
    (robot.deliveries ++ santa.deliveries).values.size


  override lazy val answer1: Int = solve1(input.toVector)
  override lazy val answer2: Int = solve2(input.toVector)
