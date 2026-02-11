package aoc2022

import nmcb.*

object Day09 extends AoC:

  enum Direction derives CanEqual:
    case U
    case D
    case R
    case L

  import Direction.*
  
  case class Position(x: Int, y: Int) derives CanEqual:

    infix def move(d: Direction): Position =
      d match
        case U => copy(y = y + 1)
        case D => copy(y = y - 1)
        case L => copy(x = x - 1)
        case R => copy(x = x + 1)

    private def alignment(p: Position): List[Direction] =
      val hor = x.compare(p.x) match
        case -1 if p.x - x >= 2 => // x < p.x
          y.compare(p.y) match
            case -1 => List(R,U)   // y < p.y
            case  0 => List(R)     // y = p.y
            case  1 => List(R,D)   // y > p.y

        case 1 if x - p.x >= 2 =>  // x > p.x
          y.compare(p.y) match
            case -1 => List(L,U)   // y < p.y
            case  0 => List(L)     // y = p.y
            case  1 => List(L,D)   // y > p.y

        case _ => List()

      val ver = y.compare(p.y) match
        case -1 if p.y - y >= 2 => // y < p.y
          x.compare(p.x) match
            case -1 => List(U,R)   // x < p.x
            case  0 => List(U)     // x = p.x
            case  1 => List(U,L)   // x > p.x

        case 1 if y - p.y >= 2 =>  // y > p.y
          x.compare(p.x) match
            case -1 => List(D,R)   // x < p.x
            case  0 => List(D)     // x = p.x
            case  1 => List(D,L)   // x > p.x

        case _ => List()

      List(hor,ver).flatten.distinct

    def follow(h: Position): Position =
      alignment(h).foldLeft(this)(_ move _)

  object Position:
    
    def of(x: Int, y: Int): Position = Position(x,y)

  case class Command(direction: Direction, steps: Int)

  lazy val commands: Vector[Command] = lines.map:
      case s"U $s" => Command(U, s.toInt)
      case s"D $s" => Command(D, s.toInt)
      case s"L $s" => Command(L, s.toInt)
      case s"R $s" => Command(R, s.toInt)

  case class Bacterium(strep: List[Position]):

    private def step(d: Direction, s: List[Position]): List[Position] =
      val nh = s.head.move(d)
      s.tail.foldLeft(List(nh))((a,t) => a :+ t.follow(a.last))

    infix def move(cmd: Command): List[Bacterium] =
      List
        .fill(cmd.steps)(cmd.direction)
        .foldLeft(List(this))((rs,d) => rs :+ Bacterium(step(d, rs.last.strep)))

  object Bacterium:

    def of(size: Int): Bacterium =
      Bacterium(List.fill(size)(Position.of(0,0)))

    def solve(commands: Vector[Command], size: Int): Int =
      commands
        .foldLeft(List(Bacterium.of(size)))((p, c) => p ++ p.last.move(c))
        .map(_.strep.last)
        .distinct
        .size


  override lazy val answer1: Int = Bacterium.solve(commands, 2)
  override lazy val answer2: Int = Bacterium.solve(commands, 10)
