package aoc2015

import nmcb.*
import nmcb.parsing.*

import scala.annotation.tailrec

object Day12 extends AoC:

  object Json:

    def string: P[String] =
      for _ <- char('"') ; s <- satisfy(_.isLetter).zeroOrMore ; _ <- char('"') yield s.mkString

    def number: P[Long] =
      (for _ <- char('-') ; i <- digits yield -i) | digits

    def member: P[(String, Json)] =
      for k <- string ; _ <- char(':') ; v <- json yield (k,v)

    def obj: P[Json] =
      for _ <- char('{') ; ms <- separated(',', member) ; _ <- char('}') yield Obj(ms.toMap)

    def arr: P[Json] =
      for _ <- char('[') ; es <- separated(',', json) ; _ <- char(']') yield Arr(es)

    def num: P[Json] =
      number.map(i => Num(i))

    def str: P[Json] =
      string.map(s => Str(s))

    def json: P[Json] =
      arr | obj | num | str

    def parse(s: String): Json =
      json.run(s)

  enum Json derives CanEqual:
    case Str(underlying: String)           extends Json
    case Num(underlying: Long)             extends Json
    case Arr(underlying: List[Json])       extends Json
    case Obj(underlying: Map[String,Json]) extends Json
  
  import Json.*

  def solve(json: Json, objValueFilter: Json => Boolean = _ => true): Long =
    @tailrec
    def loop(count: Long, stack: List[Json]): Long =
      stack match
        case Nil =>
          count
        case json :: rest =>
          json match
            case Str(_) =>
              loop(count, rest)
            case Num(n) =>
              loop(count + n, rest)
            case Arr(js) =>
              loop(count, js ::: rest)
            case Obj(ms) =>
              if ms.values.forall(objValueFilter) then
                loop(count, ms.values.toList ::: rest)
              else loop(count, rest)

    loop(0, List(json))

  lazy val puzzle: Json = Json.parse(input)

  override lazy val answer1: Long = solve(puzzle)
  override lazy val answer2: Long = solve(puzzle, _ != Str("red"))
