package aoc2022

import nmcb.*
import nmcb.pos.*

object Day09 extends AoC:

  extension (pos: Pos)

    infix def move(dir: Dir): Pos =
      dir match
        case N => (x = pos.x, y = pos.y + 1)
        case S => (x = pos.x, y = pos.y - 1)
        case W => (x = pos.x - 1, y = pos.y)
        case E => (x = pos.x + 1, y = pos.y)

    def alignment(p: Pos): Vector[Dir] =
      val hor = pos.x.compare(p.x) match
        case -1 if p.x - pos.x >= 2 => // x < p.x
          pos.y.compare(p.y) match
            case -1 => Vector(E, N)    // y < p.y
            case  0 => Vector(E)       // y = p.y
            case  1 => Vector(E, S)    // y > p.y

        case 1 if pos.x - p.x >= 2 =>  // x > p.x
          pos.y.compare(p.y) match
            case -1 => Vector(W, N)    // y < p.y
            case  0 => Vector(W)       // y = p.y
            case  1 => Vector(W, S)    // y > p.y

        case _ => Vector()

      val ver = pos.y.compare(p.y) match
        case -1 if p.y - pos.y >= 2 => // y < p.y
          pos.x.compare(p.x) match
            case -1 => Vector(N, E)    // x < p.x
            case  0 => Vector(N)       // x = p.x
            case  1 => Vector(N, W)    // x > p.x

        case 1 if pos.y - p.y >= 2 =>  // y > p.y
          pos.x.compare(p.x) match
            case -1 => Vector(S, E)    // x < p.x
            case  0 => Vector(S)       // x = p.x
            case  1 => Vector(S, W)    // x > p.x

        case _ => Vector()

      Vector(hor, ver).flatten.distinct

    def follow(h: Pos): Pos =
      alignment(h).foldLeft(pos)(_ move _)

  case class Command(direction: Dir, steps: Int)

  val commands: Vector[Command] = lines.collect:
    case s"U $s" => Command(N, s.toInt)
    case s"D $s" => Command(S, s.toInt)
    case s"L $s" => Command(W, s.toInt)
    case s"R $s" => Command(E, s.toInt)

  case class Bacterium(strep: Vector[Pos]):

    private def step(dir: Dir, strep: Vector[Pos]): Vector[Pos] =
      strep
        .tail
        .foldLeft(Vector(strep.head.move(dir))): (result, t) =>
          result :+ t.follow(result.last)

    infix def move(cmd: Command): Vector[Bacterium] =
      Vector
        .fill(cmd.steps)(cmd.direction)
        .foldLeft(Vector(this)): (result, dir) =>
          result :+ Bacterium(step(dir, result.last.strep))

  object Bacterium:

    def of(size: Int): Bacterium =
      Bacterium(Vector.fill(size)((0, 0)))

    def solve(commands: Vector[Command], size: Int): Int =
      commands
        .foldLeft(Vector(Bacterium.of(size))): (bacteria, command) =>
          bacteria ++ bacteria.last.move(command)
        .map(_.strep.last)
        .distinct
        .size


  override lazy val answer1: Int = Bacterium.solve(commands, 2)
  override lazy val answer2: Int = Bacterium.solve(commands, 10)
