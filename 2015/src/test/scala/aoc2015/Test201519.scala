package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201519 extends AnyFunSuite:

  test("2015 Day 19: Medicine for Rudolph"):
    assertResult(535)(Day19.answer1)
    assertResult(212)(Day19.answer2)
