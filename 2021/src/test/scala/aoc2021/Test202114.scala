package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202114 extends AnyFunSuite:
  
  test("Day 14: Extended Polymerization"):
    assertResult(2194)(Day14.answer1)
    assertResult(2360298895777L)(Day14.answer2)
