package aoc2015

import nmcb.*
import nmcb.parsing.*

object Day12 extends AoC:

  object Json:

    val string: P[String] =
      for _ <- char('"') ; s <- satisfy(_.isLetter).zeroOrMore ; _ <- char('"') yield s.mkString

    val number: P[Long] =
      (for _ <- char('-') ; i <- digits yield -i) | digits

    val member: P[(String,Json)] =
      for k <- string ; _ <- char(':') ; v <- json yield (k,v)

    val obj: P[Json] =
      for _ <- char('{') ; ms <- separated(',', member) ; _ <- char('}') yield Obj(ms.toMap)

    val arr: P[Json] =
      for _ <- char('[') ; es <- separated(',', json) ; _ <- char(']') yield Arr(es)

    val num: P[Json] =
      number.map(i => Num(i))

    val str: P[Json] =
      string.map(s => Str(s))

    val json: P[Json] =
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
    def loop(acc: Long, json: Json): Long =
      json match
        case Str(_)  => acc
        case Num(n)  => acc + n
        case Arr(js) => js.foldLeft(acc)(loop)
        case Obj(ms) => if ms.values.forall(objValueFilter) then ms.values.foldLeft(acc)(loop) else acc
    loop(0, json)

  lazy val puzzle: Json = Json.parse(input)

  override lazy val answer1: Long = solve(puzzle)
  override lazy val answer2: Long = solve(puzzle, _ != Str("red"))
