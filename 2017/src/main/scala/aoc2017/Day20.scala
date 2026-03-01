package aoc2017

import nmcb.*
import nmcb.predef.*

object Day20 extends AoC:

  case class Vec(x: Int, y: Int,z: Int):

    infix def +(that: Vec): Vec =
      Vec(x + that.x, y + that.y, z + that.z)

    infix def manhattanUnitDistance: Int =
      x.abs + y.abs + z.abs

  case class Particle(position: Vec, velocity: Vec, acceleration: Vec):

    def update: Particle =
      copy(position = position + velocity + acceleration, velocity = velocity + acceleration)
  
  extension (particles: Vector[Particle])

    def next: Vector[Particle] =
      particles.map(_.update)

    def minDistanceParticleIndex: Int =
      particles.zipWithIndex.minBy((particle, _) => particle.position.manhattanUnitDistance).index

    def particlesWithCollisions: Vector[Particle] =
      particles.map(_.update).groupBy(_.position).filter((_, collided) => collided.size == 1).values.flatten.toVector

  val particles: Vector[Particle] =
    lines.collect:
      case s"p=<$px,$py,$pz>, v=<$vx,$vy,$vz>, a=<$ax,$ay,$az>" =>
        val p = Vec(px.toInt, py.toInt, pz.toInt)
        val v = Vec(vx.toInt, vy.toInt, vz.toInt)
        val a = Vec(ax.toInt, ay.toInt, az.toInt)
        Particle(p, v, a)

  /** assume 500 simulation iterations to be enough */
  override lazy val answer1: Int = Iterator.iterate(particles)(_.next).map(minDistanceParticleIndex).nth(500)

  /** assume 100 simulation iterations to be enough */
  override lazy val answer2: Int = Iterator.iterate(particles)(particlesWithCollisions).nth(100).size
