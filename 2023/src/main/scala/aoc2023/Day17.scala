package aoc2023

import nmcb.*
import nmcb.predef.*
import nmcb.pos.{*, given}

object Day17 extends AoC:

  type Crucible = (current: Pos, direction: Pos, steps: Int)

  extension (city: Grid[Int])

    def leastHeatLoss(canMove: Crucible => Pos => Boolean, canStop: Crucible => Boolean): Option[Int] =

      def reachable(crucible: Crucible): Set[(Crucible, Int)] =
        for
          direction <- Pos.offset4.filterNot(_ == -crucible.direction)
          if canMove(crucible)(direction)
          next = crucible.current + direction
          if city.within(next)
          steps = if direction == crucible.direction then crucible.steps + 1 else 1
        yield
          (current = next, direction = direction, steps = steps) -> city.peek(next)

      val start  = (current = Pos.origin, direction = Pos.origin, steps = 0)
      val target = (n: Crucible) => n.current == Pos.of(city.sizeX - 1, city.sizeY - 1) && canStop(n)

      Dijkstra.run(start, target, reachable).map(_.right)

  extension (c: Crucible)

    def canMove1(dir: Pos): Boolean =
      c.steps < 3 || dir != c.direction

    def canStop1: Boolean =
      true

    def canMove2(dir: Pos): Boolean =
      (dir == c.direction && c.steps < 10) || (dir != c.direction && c.steps >= 4) || c.direction == Pos.origin

    def canStop2: Boolean =
      c.steps >= 4


  lazy val city: Grid[Int] = Grid.fromLines(lines).map(_.asDigit)
  
  override lazy val answer1: Int = city.leastHeatLoss(_.canMove1, _.canStop1).get
  override lazy val answer2: Int = city.leastHeatLoss(_.canMove2, _.canStop2).get
