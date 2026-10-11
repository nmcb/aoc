package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202101 extends AnyFunSuite:
  
  test("Day 01: Sonar Sweep"):
    assertResult(1602)(Day01.answer1)
    assertResult(1633)(Day01.answer2)
