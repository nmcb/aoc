package aoc2019

import nmcb.*
import scala.annotation.tailrec

object Day06 extends AoC:

  type Name = String

  case class Planet(name: Name, center: Name)

  val planets: List[Planet] =
    lines
      .map:
        case s"$center)$name" => Planet(name, center)
      .toList

  def find(p: Planet => Boolean): Planet =
    planets.find(p).getOrElse(sys.error("boom"))
  
  def orbits(p: Planet): List[Name] =
    @tailrec
    def go(p: Planet, acc: List[Name] = List.empty): List[Name] =
      if (p.center == "COM")
        p.name :: acc
      else
        val next = find(_.name == p.center)
        go(next, next.name :: acc)

    go(p)

  def pathToCom(n: Name): List[Name] =
    @tailrec
    def go(n: Name, acc: List[Name] = List.empty): List[Name] =
      val c = find(_.name == n).center
      if (c == "COM")
        acc
      else
        go(c, c :: acc)
    go(n)

  @tailrec
  def path(l: List[Name], r: List[Name]): List[Name] =
    (l,r) match
      case (l1 :: l2 :: _ , r1 :: r2 :: _) if l1 == r1 && l2 == r2 => path(l.tail, r.tail)
      case (l1 :: _       , r1 :: _      ) if l1 == r1             => l.reverse ++ r.tail
      case _                                                       => sys.error(s"unmatched l=$l, r=$r")

  lazy val answer1: Int = planets.map(orbits).map(_.length).sum
  lazy val answer2: Int = path(pathToCom("YOU"), pathToCom("SAN")).length
