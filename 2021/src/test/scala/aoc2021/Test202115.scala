package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202115 extends AnyFunSuite:
  
  test("Day 15: Chiton"):
    assertResult(652)(Day15.answer1)
    assertResult(2938)(Day15.answer2)
