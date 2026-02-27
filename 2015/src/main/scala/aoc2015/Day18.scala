package aoc2015

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day18 extends AoC:

  type Light = Char
  val on: Light  = '#'
  val off: Light = '.'
  
  extension (l: Light) 

    def isOn:  Boolean = l == on
    def isOff: Boolean = l == off

    def nextState(neighboursOn: Int): Light =
      if isOn then
        if neighboursOn == 2 || neighboursOn == 3 then on else off
      else
        if neighboursOn == 3 then on else off
  

  /** encapsulates the animation of a light configuration */
  case class Lights(grid: Grid[Char], overlay: Set[Pos] = Set.empty):

    def withOverlay(on: Pos*): Lights =
      copy(overlay = Set.from(on))

    def count: Int =
      grid.positions.foldLeft(0): (result, p) =>
        if light(p).isOn then result + 1 else result

    def light(p: Pos): Light =
      if overlay.contains(p) then on else grid.peek(p)

    def neighbours(p: Pos): Vector[Light] =
      p.adjoint8.toVector.filter(grid.within).map(light)

    def next: Lights =
      copy(
        grid = grid.mapElement: (p, l) =>
          val neighboursOn = neighbours(p).count(_.isOn)
          light(p).nextState(neighboursOn)
      )

    @tailrec
    final def animate(steps: Int, lights: Lights = this): Lights =
      if steps <= 0 then lights else animate(steps = steps - 1, lights = lights.next)

  val lights: Lights = Lights(Grid.fromLines(lines))

  import lights.grid.*

  override lazy val answer1: Int = lights.animate(100).count
  override lazy val answer2: Int = lights.withOverlay(leftUpper, leftBottom, rightUpper, rightBottom).animate(100).count
