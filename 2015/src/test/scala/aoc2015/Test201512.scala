package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201512 extends AnyFunSuite:

  test("2015 Day 12: JSAbacusFramework.io"):
    assertResult(111754)(Day12.answer1)
    assertResult(65402)(Day12.answer2)
