package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201520 extends AnyFunSuite:

  test("2015 Day 20: Infinite Elves and Infinite Houses"):
    assertResult(831600)(Day20.answer1)
    assertResult(884520)(Day20.answer2)
