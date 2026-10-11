package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202121 extends AnyFunSuite:
  
  test("Day 21: Dirac Dice"):
    assertResult(679329)(Day21.answer1)
    assertResult(433315766324816L)(Day21.answer2)
