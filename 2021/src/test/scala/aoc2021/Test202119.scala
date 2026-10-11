package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202119 extends AnyFunSuite:
  
  test("Day 19: Beacon Scanner"):
    assertResult(320)(Day19.answer1)
    assertResult(9655)(Day19.answer2)
