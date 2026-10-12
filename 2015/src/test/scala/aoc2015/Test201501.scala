package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201501 extends AnyFunSuite:

  test("2015 Day 01: Not Quite Lisp"):
    assertResult(138)(Day01.answer1)
    assertResult(1771)(Day01.answer2)
