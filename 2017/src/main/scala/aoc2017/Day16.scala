package aoc2017

import nmcb.*

object Day16 extends AoC:

  type Programs = String

  object Programs:
    def init: Programs = "abcdefghijklmnop"

  extension (ps: Programs)

    def spin(len: Int): Programs =
      ps.takeRight(len) ++ ps.take(ps.size - len)

    def exchange(a: Int, b: Int): Programs =
      val pa = ps(a)
      val pb = ps(b)
      ps.updated(a, pb).updated(b, pa)

    def partner(pa: Char, pb: Char): Programs =
      val a = ps.indexOf(pa)
      val b = ps.indexOf(pb)
      exchange(a, b)

    infix def move(m: String): Programs =
      m match
        case s"s$len"    => spin(len.toInt)
        case s"x$a/$b"   => exchange(a.toInt, b.toInt)
        case s"p$pa/$pb" => partner(pa.head, pb.head)

    infix def dance(ms: Vector[String]): Programs =
      ms.foldLeft(ps)(_ move _)

  val moves: Vector[String] = input.split(",").toVector

  override lazy val answer1: String = Programs.init.dance(moves)
  override lazy val answer2: String = Cycle.find(Programs.init, _.dance(moves)).simulate(1000000000L)
