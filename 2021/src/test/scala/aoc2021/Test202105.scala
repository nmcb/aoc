package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202105 extends AnyFunSuite:
  
  test("Day 05: Hydrothermal Venture"):
    assertResult(7085)(Day05.answer1)
    assertResult(20271)(Day05.answer2)
