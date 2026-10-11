package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202110 extends AnyFunSuite:
  
  test("2021 Day 10: Syntax Scoring"):
    assertResult(268845L)(Day10.answer1)
    assertResult(4038824534L)(Day10.answer2)

