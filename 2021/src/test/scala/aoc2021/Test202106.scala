package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202106 extends AnyFunSuite:
  
  test("2021 Day 06: Lanternfish"):
    assertResult(380612)(Day06.answer1)
    assertResult(1710166656900L)(Day06.answer2)
