package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201521 extends AnyFunSuite:

  test("2015 Day 21: RPG Simulator 20XX"):
    assertResult(111)(Day21.answer1)
    assertResult(188)(Day21.answer2)
