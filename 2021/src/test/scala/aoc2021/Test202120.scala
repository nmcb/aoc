package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202120 extends AnyFunSuite:
  
  test("2021 Day 20: Trench Map"):
    assertResult(5298)(Day20.answer1)
    assertResult(17548)(Day20.answer2)
