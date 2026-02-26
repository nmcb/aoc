package aoc2019

import nmcb.*
import nmcb.pos.*
import scala.annotation.tailrec

object Day11 extends AoC:

  import cpu.*

  type Panels = Map[Pos,Long]

  object Panels:
    val empty: Panels = Map.empty.withDefaultValue(0L)

  case class Robot(cpu: CPU, pos: Pos = (0, 0), dir: Dir = N, panels: Panels = Panels.empty):
    @tailrec
    final def paint: Panels =
      cpu.withInput(panels(pos)).outputStates match
        case LazyList() =>
          panels
        case LazyList((_,color),(next,turn)) =>
          val rotate = if turn == 1 then dir.cw else dir.ccw
          Robot(
            cpu    = next,
            pos    = pos step rotate,
            dir    = rotate,
            panels = panels.updated(pos, color)
          ).paint

  val robot: Robot =
    val program: Mem = Mem.parse(input)
    Robot(
      cpu    = CPU(program),
      pos    = (0, 0),
      dir    = N,
      panels = Panels.empty
    )

  extension (panels: Panels)
    def asString: String =
      val minX = panels.keys.map(_.x).min
      val maxX = panels.keys.map(_.x).max
      val minY = panels.keys.map(_.y).min
      val maxY = panels.keys.map(_.y).max

      val sb = StringBuffer()
      for y <- minY to maxY do
        for x <- minX to maxX do
          sb.append(if panels((x, y)) == 1 then '█' else '░')
        sb.append("\n")
      sb.toString

  override lazy val answer1: Int    = robot.paint.size
  override lazy val answer2: String = robot.copy(panels = Map((0, 0) -> 1L).withDefaultValue(0L)).paint.asString
