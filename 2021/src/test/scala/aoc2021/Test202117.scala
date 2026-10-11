package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202117 extends AnyFunSuite:
  
  test("Day 17: Trick Shot"):
    assertResult(9180)(Day17.answer1)
    assertResult(3767)(Day17.answer2)
