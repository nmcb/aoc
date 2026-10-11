package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202118 extends AnyFunSuite:
  
  test("Day 18: Snailfish"):
    assertResult(3935)(Day18.answer1)
    assertResult(4669)(Day18.answer2)
