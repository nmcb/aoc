package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201517 extends AnyFunSuite:

  test("2015 Day 17: No Such Thing as Too Much"):
    assertResult(4372)(Day17.answer1)
    assertResult(4)(Day17.answer2)

