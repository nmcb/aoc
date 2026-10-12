package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201507 extends AnyFunSuite:

  test("2015 Day 07: Some Assembly Required"):
    assertResult(956)(Day07.answer1)
    assertResult(40149)(Day07.answer2)
