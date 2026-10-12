package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201509 extends AnyFunSuite:

  test("2015 Day 09: All in a Single Night"):
    assertResult(207)(Day09.answer1)
    assertResult(804)(Day09.answer2)
