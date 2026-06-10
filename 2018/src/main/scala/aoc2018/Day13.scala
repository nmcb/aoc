package aoc2018

import nmcb.*
import nmcb.predef.*
import nmcb.pos.{*, given}

import scala.annotation.tailrec

object Day13 extends AoC:

  enum Turn derives CanEqual:
    case Left
    case Straight
    case Right

    def next: Turn =
      this match
        case Left     => Straight
        case Straight => Right
        case Right    => Left

  import Turn.*

  extension (d: Dir)

    infix def follow(c: Char): Dir =
      (d, c).runtimeChecked match
        case (_, '|') | (_, '-') => d
        case (N, '/')            => E
        case (E, '/')            => N
        case (S, '/')            => W
        case (W, '/')            => S
        case (N, '\\')           => W
        case (E, '\\')           => S
        case (S, '\\')           => E
        case (W, '\\')           => N

    infix def turn(turn: Turn): Dir =
      (d, turn) match
        case (N, Left)     => W
        case (E, Left)     => N
        case (S, Left)     => E
        case (W, Left)     => S
        case (N, Right)    => E
        case (E, Right)    => S
        case (S, Right)    => W
        case (W, Right)    => N
        case (_, Straight) => d

  case class Cart(pos: Pos, dir: Dir, atIntersection: Turn = Left):

    def move(grid: Grid[Char]): Cart =
      val c = grid.peekOption(pos step dir)
      c.runtimeChecked match
        case Some('|') | Some('-')  => copy(pos = pos step dir)
        case Some('/') | Some('\\') => copy(pos = pos step dir, dir = dir follow c.get)
        case Some('+')              => copy(pos = pos step dir, dir = dir turn atIntersection, atIntersection = atIntersection.next)

  val (grid: Grid[Char], carts: Vector[Cart]) =

    val matrix = Grid.fromLines(lines)

    val carts =
      for
        y <- (0 until matrix.sizeY).toVector
        x <- (0 until matrix.sizeX).toVector
      yield
        matrix.peekOption((x, y)) match
          case Some(c) if c == '^' => Some(Cart((x, y), N))
          case Some(c) if c == '>' => Some(Cart((x, y), E))
          case Some(c) if c == 'v' => Some(Cart((x, y), S))
          case Some(c) if c == '<' => Some(Cart((x, y), W))
          case _                   => None

    val grid =
      matrix.map:
        case '^' | 'v' => '|'
        case '<' | '>' => '-'
        case c         => c


    (grid, carts.flatten)

  /**
   * Don't move all carts per iteration, or we'll miss collisions that pass each other, e.g.:
   *
   * # step 1
   * --><--
   *
   * # step 2
   * --<>--
   *
   */
  def solve1(value: Grid[Char], carts: Vector[Cart]): Pos =
    @tailrec
    def loop(todo: Vector[Cart], done: Vector[Cart] = Vector.empty): Pos =
      val counts = (todo ++ done).map(_.pos).countElements
      if counts.exists(_.count > 1) then
        counts.maxBy(_.count).pos
      else if todo.isEmpty then
        loop(todo = done.sortBy(_.pos))
      else
        loop(todo = todo.tail, done = todo.head.move(grid) +: done)
    loop(carts.sortBy(_.pos))


  def solve2(value: Grid[Char], carts: Vector[Cart]): Pos =
    @tailrec
    def loop(todo: Vector[Cart], done: Vector[Cart] = Vector.empty): Pos =
      if (todo ++ done).size == 1 then
        /** don't forget to move the last cart one tick */
        (todo ++ done).head.move(grid).pos
      else if todo.isEmpty then
        loop(todo = done.sortBy(_.pos))
      else
        val moved = todo.head.move(grid)
        val counts = (moved +: (todo.tail ++ done)).map(_.pos).countElements
        counts.find(_.count > 1) match
          case Some(pos, _) => loop(todo = todo.tail.filterNot(_.pos == pos), done = done.filterNot(_.pos == pos))
          case None         => loop(todo = todo.tail, done = moved +: done)
    loop(carts.sortBy(_.pos))

  extension (pos: Pos)

    def asString: String =
      s"${pos.x},${pos.y}"

  override lazy val answer1: String = solve1(grid, carts).asString
  override lazy val answer2: String = solve2(grid, carts).asString
