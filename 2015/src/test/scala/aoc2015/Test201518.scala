package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201518 extends AnyFunSuite:

  test("2015 Day 18: Like a GIF For Your Yard"):
    assertResult(814)(Day18.answer1)
    assertResult(924)(Day18.answer2)
