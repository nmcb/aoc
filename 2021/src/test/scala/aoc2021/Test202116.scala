package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202116 extends AnyFunSuite:
  
  test("2021 Day 16: Packet Decoder"):
    assertResult(886)(Day16.answer1)
    assertResult(184487454837L)(Day16.answer2)
