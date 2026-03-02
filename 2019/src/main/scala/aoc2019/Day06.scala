package aoc2019

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day06 extends AoC:

  type Name = String

  case class Planet(name: Name, center: Name)

  extension (planets: Vector[Planet])

    def orbits: Map[Name, Vector[Name]] =
      @tailrec
      def loop(p: Planet, acc: Vector[Name] = Vector.empty): Vector[Name] =
        if (p.center == "COM")
          p.name +: acc
        else
          val next = planets.find(_.name == p.center).get
          loop(next, next.name +: acc)
      planets.map(p => p.name -> loop(p)).toMap

    def pathToCom(n: Name): Vector[Name] =
      @tailrec
      def loop(n: Name, acc: Vector[Name] = Vector.empty): Vector[Name] =
        val c = planets.find(_.name == n).get.center
        if (c == "COM")
          acc
        else
          loop(c, c +: acc)
      loop(n)

  @tailrec
  def path(l: Vector[Name], r: Vector[Name]): Vector[Name] =
    (l, r).runtimeChecked match
      case (l1 +: l2 +: _ , r1 +: r2 +: _) if l1 == r1 && l2 == r2 => path(l.tail, r.tail)
      case (l1 +: _       , r1 +: _      ) if l1 == r1             => l.reverse ++ r.tail

  
  val planets: Vector[Planet] = lines.collect:
    case s"$center)$name" => Planet(name, center)
  
  override lazy val answer1: Int = planets.orbits.map(_.right.length).sum
  override lazy val answer2: Int = path(planets.pathToCom("YOU"), planets.pathToCom("SAN")).length
