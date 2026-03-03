package aoc2019

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day12 extends AoC:

  case class Vec(x: Int, y: Int, z: Int):

    infix def +(that: Vec): Vec = copy(x = x + that.x, y = y + that.y, z = z + that.z)
    infix def -(that: Vec): Vec = copy(x = x - that.x, y = y - that.y, z = z - that.z)

    def sign: Vec = Vec(x.sign, y.sign, z.sign)
    def manhattan: Int = x.abs + y.abs + z.abs

  object Vec:
    val zero: Vec = Vec(0, 0, 0)

  case class Moon(position: Vec, velocity: Vec):
    def energy: Int =
      position.manhattan * velocity.manhattan

  val moons: Vector[Moon] =
    lines
      .map:
        case s"<x=$x, y=$y, z=$z>" => Moon(Vec(x.toInt, y.toInt, z.toInt), Vec.zero)

  extension (moons: Vector[Moon])

    def step: Vector[Moon] =
      moons.map: moon =>
        val velocity = moons.foldLeft(moon.velocity): (result,other) =>
          result + (other.position - moon.position).sign
        Moon(moon.position + velocity, velocity)

  /**
   * The key insights are:  (1) The axes (x, y, z) are totally independent so it suffices to find
   * the period for each axis separately and the answer is the lcm of these.  (2) Each axis will
   * repeat "relatively quickly" (e.g. fast enough to brute force).  And (3) since each state
   * has a unique parent, the first repeat must be a repeat of the initial state.
   */
  extension (moons: Vector[Moon])

    def periodOf(axis: Vec => Int): Long =

      def same(vs: Vector[(Vec, Vec)]): Boolean =
        vs.forall((a, b) => axis(a) == axis(b))

      @tailrec
      def loop(current: Vector[Moon], count: Long = 1): Long =
        val next    = current.step
        val samePos = same(next.zip(moons).map((a, b) => (a.position, b.position)))
        val sameVel = same(next.zip(moons).map((a, b) => (a.velocity, b.velocity)))
        if samePos && sameVel then count else loop(current = next, count = count + 1)
      loop(moons)

    def period: Long =
      val periodX = periodOf(_.x)
      val periodY = periodOf(_.y)
      val periodZ = periodOf(_.z)
      periodX lcm periodY lcm periodZ

  import Iterator.*

  override lazy val answer1: Int  = iterate(moons)(_.step).nth(1000).map(_.energy).sum
  override lazy val answer2: Long = moons.period
