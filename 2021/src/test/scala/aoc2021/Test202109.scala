package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202109 extends AnyFunSuite:
  
  test("2021 Day 09: Smoke Basin"):
    assertResult(502)(Day09.answer1)
    assertResult(1330560)(Day09.answer2)
