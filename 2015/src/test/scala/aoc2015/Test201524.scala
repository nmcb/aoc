package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201524 extends AnyFunSuite:

  test("2015 Day 24: It Hangs in the Balance"):
    assertResult(11846773891L)(Day24.answer1)
    assertResult(80393059L)(Day24.answer2)
