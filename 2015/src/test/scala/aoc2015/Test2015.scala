package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test2015 extends AnyFunSuite:

  test("Day 24: It Hangs in the Balance"):
    assertResult(11846773891L)(Day24.answer1)
    assertResult(80393059L)(Day24.answer2)

  test("Day 25: Let It Snow"):
    assertResult(19980801)(Day25.answer1)
