package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202108 extends AnyFunSuite:
  
  test("2021 Day 08: Seven Segment Search"):
    assertResult(521)(Day08.answer1)
    assertResult(1016804)(Day08.answer2)
