package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201522 extends AnyFunSuite:

  test("2015 Day 22: Wizard Simulator 20XX"):
    assertResult(1824)(Day22.answer1)
    assertResult(1937)(Day22.answer2)
