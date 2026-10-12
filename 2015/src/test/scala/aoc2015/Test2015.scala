package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test2015 extends AnyFunSuite:

  test("Day 21: RPG Simulator 20XX"):
    assertResult(111)(Day21.answer1)
    assertResult(188)(Day21.answer2)

  test("Day 22: Wizard Simulator 20XX"):
    assertResult(1824)(Day22.answer1)
    assertResult(1937)(Day22.answer2)

  test("Day 23: Opening the Turing Lock"):
    assertResult(255)(Day23.answer1)
    assertResult(334)(Day23.answer2)

  test("Day 24: It Hangs in the Balance"):
    assertResult(11846773891L)(Day24.answer1)
    assertResult(80393059L)(Day24.answer2)

  test("Day 25: Let It Snow"):
    assertResult(19980801)(Day25.answer1)
