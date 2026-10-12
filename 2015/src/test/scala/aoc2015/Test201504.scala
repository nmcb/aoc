package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201504 extends AnyFunSuite:

  test("2015 Day 04: The Ideal Stocking Stuffer"):
    assertResult(282749)(Day04.answer1)
    assertResult(9962624)(Day04.answer2)
