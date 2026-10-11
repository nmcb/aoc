package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202102 extends AnyFunSuite:
  
  test("2021 Day 02: Dive!"):
    assertResult(2322630)(Day02.answer1)
    assertResult(2105273490)(Day02.answer2)
