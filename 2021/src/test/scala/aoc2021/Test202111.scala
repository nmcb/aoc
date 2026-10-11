package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202111 extends AnyFunSuite:
  
  test("2021 Day 11: Dumbo Octopus"):
    assertResult(1732)(Day11.answer1)
    assertResult(290)(Day11.answer2)
