package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201513 extends AnyFunSuite:

  test("2015 Day 13: Knights of the Dinner Table"):
    assertResult(733)(Day13.answer1)
    assertResult(725)(Day13.answer2)
