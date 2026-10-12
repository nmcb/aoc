package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201516 extends AnyFunSuite:

  test("2015 Day 16: Aunt Sue"):
    assertResult(40)(Day16.answer1)
    assertResult(241)(Day16.answer2)

