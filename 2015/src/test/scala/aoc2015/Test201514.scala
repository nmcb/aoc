package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201514 extends AnyFunSuite:

  test("2015 Day 14: Reindeer Olympics"):
    assertResult(2660)(Day14.answer1)
    assertResult(1256)(Day14.answer2)
