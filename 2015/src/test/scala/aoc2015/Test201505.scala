package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Test201505 extends AnyFunSuite:

  test("2015 Day 05: Doesn't He Have Intern-Elves For This?"):
    assertResult(258)(Day05.answer1)
    assertResult(53)(Day05.answer2)
