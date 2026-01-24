package aoc2019

import nmcb.*

import scala.annotation.tailrec

object Day06 extends AoC:

  type Name = String

  case class Planet(name: Name, center: Name)

  val planets: Vector[Planet] =
    lines.map:
      case s"$center)$name" => Planet(name, center)

  def find(p: Planet => Boolean): Planet =
    planets.find(p).getOrElse(sys.error("boom"))

  def orbits(p: Planet): Vector[Name] =
    @tailrec
    def loop(p: Planet, acc: Vector[Name] = Vector.empty): Vector[Name] =
      if (p.center == "COM")
        p.name +: acc
      else
        val next = find(_.name == p.center)
        loop(next, next.name +: acc)

    loop(p)

  def pathToCom(n: Name): Vector[Name] =
    @tailrec
    def loop(n: Name, acc: Vector[Name] = Vector.empty): Vector[Name] =
      val c = find(_.name == n).center
      if (c == "COM")
        acc
      else
        loop(c, c +: acc)
    loop(n)

  @tailrec
  def path(l: Vector[Name], r: Vector[Name]): Vector[Name] =
    (l,r) match
      case (l1 +: l2 +: _ , r1 +: r2 +: _) if l1 == r1 && l2 == r2 => path(l.tail, r.tail)
      case (l1 +: _       , r1 +: _      ) if l1 == r1             => l.reverse ++ r.tail
      case _                                                       => sys.error(s"unmatched l=$l, r=$r")

  override lazy val answer1: Int = planets.map(orbits).map(_.length).sum
  override lazy val answer2: Int = path(pathToCom("YOU"), pathToCom("SAN")).length
