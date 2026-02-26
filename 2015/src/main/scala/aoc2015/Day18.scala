package aoc2015

import nmcb.*
import nmcb.pos.{*, given}

import scala.annotation.tailrec

object Day18 extends AoC:

  type Light = Char
  val on: Light  = '#'
  val off: Light = '.'
  
  extension (l: Light) 

    def isOn:  Boolean = l == on
    def isOff: Boolean = l == off

    def next(neighbouring: Vector[Light]): Light =
      val n = neighbouring.count(_.isOn)

      if isOn then
        if n == 2 || n == 3 then on else off
      else
        if n == 3 then on else off
  
  type Row = Vector[Light]

  object Row:
    def empty: Row =
      Vector.empty[Light]

  /** zero based top-to-bottom-left-to-right indexed, ie. [y0, y1 .. yn][x0, x1 .. xn] */
  type Conf = Vector[Row]

  object Conf:
    def empty: Conf =
      Vector.empty[Row]

  /** grid encapsulates the animation of a light configuration */
  case class Grid(conf: Conf, overlay: Set[Pos] = Set.empty):
    val sizeX: Int = conf.map(_.size).max
    val sizeY: Int = conf.size
    val minX: Int  = 0
    val minY: Int  = 0
    val maxX: Int  = sizeX - 1
    val maxY: Int  = sizeY - 1

    def withOverlay(on: Pos*): Grid =
      copy(overlay = Set.from(on))

    def fold[A](zero: A)(inc: A => A)(f: (Int, Int, A) => A): A =
      (minY to maxY).foldLeft(zero): (z, y) =>
        (minX to maxX).foldLeft(inc(z)): (z, x) =>
          f(x, y, z)

    def count: Int =
      fold(0)(identity): (x, y, n) =>
        if light(x, y).isOn then n + 1 else n

    def light(x: Int, y: Int): Light =
      if overlay(y, x) then on else conf(y)(x)

    def neighbours(x: Int, y: Int): Vector[Light] =
      Pos.offset8
        .map((dx, dy) => (x + dx, y + dy))
        .filter((px, py) => px >= minX && px <= maxX && py >= minY && py <= maxY)
        .map(light)
        .toVector

    def next: Grid =
      val step: Conf =
        fold(Conf.empty)(_ :+ Row.empty)((x, y, conf) =>
          conf.init :+ (conf.last :+ light(x, y).next(neighbours(x, y))))
      copy(conf = step)

    @tailrec
    final def animate(steps: Int, grid: Grid = this): Grid =
      if steps <= 0 then grid else animate(steps = steps - 1, grid = grid.next)

  val grid: Grid =
    val initial =
      lines
        .foldLeft(Conf.empty)((conf,line) =>
          conf :+ line.foldLeft(Row.empty)((row, char) =>
            row :+ char))
    Grid(initial)

  import grid.*

  override lazy val answer1: Int = grid.animate(100).count
  override lazy val answer2: Int = grid.withOverlay((minY,minX), (minY,maxX), (maxY,minX), (maxY,maxX)).animate(100).count
