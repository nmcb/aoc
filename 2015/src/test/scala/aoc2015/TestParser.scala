package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class TestParser extends AnyFunSuite:

  test("Day12 - Json.parse"):
    import aoc2015.Day12.*
    import Json.*
    assertResult(Num(12))(parse("12"))
    assertResult(Num(-1))(parse("-1"))
    assertResult(Str(""))(parse("\"\""))
    assertResult(Str("a"))(parse("\"a\""))
    assertResult(Str("ab"))(parse("\"ab\""))
    assertResult(Arr(List(Num(1),Obj(Map("a" -> Num(1),"b" -> Num(2))))))(parse("""[1,{"a":1,"b":2}]"""))
    assertResult(Obj(Map("a" -> Num(1),"b" -> Arr(List(Num(1),Num(2))))))(parse("""{"a":1,"b":[1,2]}"""))
