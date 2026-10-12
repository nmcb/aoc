package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201523 extends AnyFunSuite:

  test("2015 Day 23: Opening the Turing Lock"):
    assertResult(255)(Day23.answer1)
    assertResult(334)(Day23.answer2)
