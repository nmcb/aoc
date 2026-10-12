package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201508 extends AnyFunSuite:

  test("2015 Day 08: Matchsticks"):
    assertResult(1333)(Day08.answer1)
    assertResult(2046)(Day08.answer2)
