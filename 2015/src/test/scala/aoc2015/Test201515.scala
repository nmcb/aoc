package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201515 extends AnyFunSuite:

  test("2015 Day 15: Science for Hungry People"):
    assertResult(13882464)(Day15.answer1)
    assertResult(11171160)(Day15.answer2)
