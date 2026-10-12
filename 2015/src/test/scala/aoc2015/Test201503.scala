package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201503 extends AnyFunSuite:

  test("2015 Day 03: Perfectly Spherical Houses in a Vacuum"):
    assertResult(2572)(Day03.answer1)
    assertResult(2631)(Day03.answer2)
