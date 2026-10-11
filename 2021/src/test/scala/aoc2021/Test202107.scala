package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202107 extends AnyFunSuite:
  
  test("Day 07: The Treachery of Whales"):
    assertResult(328187)(Day07.answer1)
    assertResult(91257582)(Day07.answer2)
