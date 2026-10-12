package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201510 extends AnyFunSuite:

  test("2015 Day 10: Elves Look, Elves Say"):
    assertResult(252594)(Day10.answer1)
    assertResult(3579328)(Day10.answer2)
