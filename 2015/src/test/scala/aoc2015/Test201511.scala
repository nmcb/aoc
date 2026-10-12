package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201511 extends AnyFunSuite:

  test("2015 Day 11: Corporate Policy"):
    assertResult("hxbxxyzz")(Day11.answer1)
    assertResult("hxcaabcc")(Day11.answer2)
