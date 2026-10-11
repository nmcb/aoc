package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202112 extends AnyFunSuite:
  
  test("Day 12: Passage Pathing"):
    assertResult(5252)(Day12.answer1)
    assertResult(147784)(Day12.answer2)
