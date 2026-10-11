package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202124 extends AnyFunSuite:
  
  test("Day 24: Arithmetic Logic Unit"):
    assertResult("39999698799429")(Day24.answer1)
    assertResult("18116121134117")(Day24.answer2)
