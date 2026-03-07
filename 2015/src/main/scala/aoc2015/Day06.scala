package aoc2015

import nmcb.*
import nmcb.pos.*

object Day06 extends AoC:

  case class Task(inst: String, minPos: Pos, maxPos: Pos):

    def influences(p: Pos): Boolean =
      p.withinBounds(minPos, maxPos)

  object Task:

    def make(cmd: String, p0x: String, p0y: String, p1x: String, p1y: String): Task =
      Task(cmd, (x = p0x.toInt, y = p0y.toInt), (x = p1x.toInt, y = p1y.toInt))

    def fromLine(s: String): Task =
      s match
        case s"toggle $p0x,$p0y through $p1x,$p1y" => Task.make("toggle", p0x, p0y, p1x, p1y)
        case s"turn on $p0x,$p0y through $p1x,$p1y" => Task.make("on", p0x, p0y, p1x, p1y)
        case s"turn off $p0x,$p0y through $p1x,$p1y" => Task.make("off", p0x, p0y, p1x, p1y)


  abstract class Lights[A](tasks: Vector[Task]):
    val zero:   A
    val toggle: A => A
    val on:     A => A
    val off:    A => A

    def instruction(a: A, task: Task): A =
      task.inst match
        case "toggle" => toggle(a)
        case "on"     => on(a)
        case "off"    => off(a)

    def runLight(p: Pos): A =
      tasks.filter(_.influences(p)).foldLeft(zero)(instruction)

    def runAllLights: Vector[A] =
      Vector.tabulate(1000, 1000)((x, y) => runLight(x, y)).flatten

  class BooleanLights(val tasks: Vector[Task]) extends Lights[Boolean](tasks):
    val zero: Boolean              = false
    val toggle: Boolean => Boolean = v => !v
    val on: Boolean => Boolean     = _ => true
    val off: Boolean => Boolean    = _ => false

  class IntLights(val tasks: Vector[Task]) extends Lights[Int](tasks):
    val zero: Int   = 0
    val toggle: Int => Int = v => v + 2
    val on: Int => Int     = v => v + 1
    val off: Int => Int    = v => if v <= 0 then 0 else v - 1


  val tasks: Vector[Task] = lines.map(Task.fromLine)

  override lazy val answer1: Int = BooleanLights(tasks).runAllLights.count(_ == true)
  override lazy val answer2: Int = IntLights(tasks).runAllLights.sum
