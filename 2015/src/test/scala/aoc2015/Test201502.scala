package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201502 extends AnyFunSuite:

  test("2015 Day 02: I Was Told There Would Be No Math"):
    assertResult(1606483)(Day02.answer1)
    assertResult(3842356)(Day02.answer2)
