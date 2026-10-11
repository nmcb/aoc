package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202104 extends AnyFunSuite:
  
  test("2021 Day 04: Giant Squid"):
    assertResult(21607)(Day04.answer1)
    assertResult(19012)(Day04.answer2)
