package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201506 extends AnyFunSuite:

  test("2015 Day 06: Probably a Fire Hazard"):
    assertResult(569999)(Day06.answer1)
    assertResult(17836115)(Day06.answer2)
