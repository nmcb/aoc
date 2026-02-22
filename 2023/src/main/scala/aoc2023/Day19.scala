package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day19 extends AoC:

  case class Part(x: Long, m: Long, a: Long, s: Long):
    def sum: Long = x + m + a + s

  enum Rule derives CanEqual:
    case Accepted
    case Rejected
    case Deferred(workflow: String)
    case Compared(select1: Part => Boolean, select2: Long => Boolean, workflow: String, selector: Char)

  import Rule.*
  
  case class Range(from: Long, to: Long):
    def size: Long = to - from + 1

  case class Search(x: Range, m: Range, a: Range, s: Range, workflow: String):
    def size: Long = x.size * m.size * a.size * s.size

  object Search:
    def full: Search =
      Search(Range(1, 4000), Range(1, 4000), Range(1, 4000), Range(1, 4000), "in")

  type Workflows = Map[String, Vector[Rule]]

  val workflows: Workflows =
    lines
      .collect:
        case s"$name{$specification}" =>
          val rules =
            specification
              .split(',')
              .map: s =>
                if s.contains('<') || s.contains('>') then
                  val selector: Part => Long =
                    s match
                      case s"x$rest" => _.x
                      case s"m$rest" => _.m
                      case s"a$rest" => _.a
                      case s"s$rest" => _.s
                  val valueStr = s.drop(2).takeWhile(_.isDigit)
                  val value    = valueStr.toLong
                  val workflow = s.drop(3 + valueStr.length)
                  s(1) match
                    case '<' => Compared(selector(_) < value, _ < value, workflow, s.head)
                    case '>' => Compared(selector(_) > value, _ > value, workflow, s.head)
                else if s == "R" then Rejected
                else if s == "A" then Accepted
                else                  Deferred(s)
              .toVector
          name -> rules
      .toMap + ("A" -> Vector(Accepted)) + ("R" -> Vector(Rejected))

  val parts: Vector[Part] =
    lines.collect:
      case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toLong, m.toLong, a.toLong, s.toLong)


  def solve1(workflow: String, workflows: Workflows)(part: Part): Boolean =
    @tailrec
    def loop(part: Part, workflow: String): Rule =
      val rules = workflows(workflow)
      val matched =
        rules
          .find:
            case Accepted                  => true
            case Rejected                  => true
            case Deferred(_)               => true
            case Compared(select, _, _, _) => select(part)
          .getOrElse(sys.error(s"no rule matched: $workflow"))

      matched match
        case Accepted                    => Accepted
        case Rejected                    => Rejected
        case Deferred(workflow)          => loop(part, workflow)
        case Compared(_, _, workflow, _) => loop(part, workflow)

    loop(part, workflow) == Accepted


  def solve2(searches: Vector[Search], workflows: Workflows): Vector[Search] =
    @tailrec
    def loop(searches: Vector[Search], found: Vector[Search] = Vector.empty): Vector[Search] =
      if searches.isEmpty then
        found
      else
        val search = searches.head
        val rules = workflows(search.workflow)

        def selector(char: Char): Search => Range =
          char match
            case 'x' => _.x
            case 'm' => _.m
            case 'a' => _.a
            case 's' => _.s

        val matched =
          rules.find:
            case Accepted => true
            case Rejected => true
            case Deferred(workflow) => true
            case Compared(_, select, workflow, char) =>
              val range = selector(char)(search)
              (range.from to range.to).exists(select)
          .getOrElse(sys.error(s"no rule matched"))

        matched match
          case Accepted => loop(searches.tail, search +: found)
          case Rejected => loop(searches.tail, found)
          case Deferred(workflow) => loop(search.copy(workflow = workflow) +: searches.tail, found)
          case Compared(_, select, workflow, char) =>
            val min = selector(char)(search).from
            val max = selector(char)(search).to
            val included = (min to max).map(select)
            val split = (1 until included.size).find(i => included(i - 1) != included(i))

            split match
              case None     => loop(search.copy(workflow = workflow) +: searches.tail, found)
              case Some(at) =>

                def make(range: Range): Vector[Range] =
                  Vector(Range(range.from, range.from + at - 1), Range(range.from + at, range.to))

                val splits =
                  char match
                    case 'x' => make(search.x).map(r => search.copy(x = r))
                    case 'm' => make(search.m).map(r => search.copy(m = r))
                    case 'a' => make(search.a).map(r => search.copy(a = r))
                    case 's' => make(search.s).map(r => search.copy(s = r))

                loop(splits ++ searches.tail, found)

    loop(searches)

  override lazy val answer1: Long =
    parts.filter(solve1("in", workflows)).map(_.sum).sum

  override lazy val answer2: Long =
    solve2(Vector(Search.full), workflows).map(_.size).sum
