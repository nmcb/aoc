package aoc2015

import nmcb.*
import nmcb.pos.*

object Day03 extends AoC:

  val commands: List[Char] = input.toList

  case class Area(start: Pos = Pos.origin, deliveries: Area.Deliveries = Area.Deliveries.init):

    private def moveToAndDeliver(loc: Pos): Area =
      import Area.*
      copy(start = loc, deliveries = deliveries.add(loc))

    infix def next(cmd: Area.Command): Area =
      cmd match
        case '>' => moveToAndDeliver(start.copy(x = start.x + 1))
        case '<' => moveToAndDeliver(start.copy(x = start.x - 1))
        case '^' => moveToAndDeliver(start.copy(y = start.y + 1))
        case 'v' => moveToAndDeliver(start.copy(y = start.y - 1))

  object Area:

    type Deliveries = Map[Pos,Int]

    object Deliveries:

      def empty: Deliveries =
        Map.empty

      def init: Deliveries =
        empty.add(Pos.origin)

    extension (d: Deliveries) def add(loc: Pos): Deliveries =
      d.updatedWith(loc)(_.orElse(Some(0)).map(_ + 1))

    type Command = Char

    def init: Area =
      Area(start = Pos.origin, deliveries = Area.Deliveries.init)


  val (robot, santa) =
    commands
      .zipWithIndex
      .foldLeft((Area.init, Area.init)) { case ((robot, santa), (command, index)) =>
        if (index % 2 != 0) (robot next command, santa) else (robot, santa next command)
      }

  lazy val answer1: Int = commands.foldLeft(Area.init)(_ next _).deliveries.values.size
  lazy val answer2: Int = (robot.deliveries ++ santa.deliveries).values.size
