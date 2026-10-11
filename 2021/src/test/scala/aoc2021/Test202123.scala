package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202123 extends AnyFunSuite:
  
  test("2021 Day 23: Amphipod"):
    assertResult(11417)(Day23.answer1)
    assertResult(49529)(Day23.answer2)
