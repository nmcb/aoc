package aoc2015

import nmcb.*

import org.scalatest.funsuite.AnyFunSuite

class Test2015 extends AnyFunSuite:

  test("Day 1: Not Quite Lisp"):
    assertResult(138)(Day01.answer1)
    assertResult(1771)(Day01.answer2)

  test("Day 2: I Was Told There Would Be No Math"):
    assertResult(1606483)(Day02.answer1)
    assertResult(3842356)(Day02.answer2)

  test("Day 3: Perfectly Spherical Houses in a Vacuum"):
    assertResult(2572)(Day03.answer1)
    assertResult(2631)(Day03.answer2)

  test("Day 4: The Ideal Stocking Stuffer"):
    assertResult(282749)(Day04.answer1)
    assertResult(9962624)(Day04.answer2)

  test("Day 5: Doesn't He Have Intern-Elves For This?"):
    assertResult(258)(Day05.answer1)
    assertResult(53)(Day05.answer2)

  test("Day 6: Probably a Fire Hazard"):
    assertResult(569999)(Day06.answer1)
    assertResult(17836115)(Day06.answer2)

  test("Day 7: Some Assembly Required"):
    assertResult(956)(Day07.answer1)
    assertResult(40149)(Day07.answer2)

  test("Day 8: Matchsticks"):
    assertResult(1333)(Day08.answer1)
    assertResult(2046)(Day08.answer2)

  test("Day 9: All in a Single Night"):
    assertResult(207)(Day09.answer1)
    assertResult(804)(Day09.answer2)

  test("Day 10: Elves Look, Elves Say"):
    assertResult(252594)(Day10.answer1)
    assertResult(3579328)(Day10.answer2)

  test("Day 11: Corporate Policy"):
    assertResult("hxbxxyzz")(Day11.answer1)
    assertResult("hxcaabcc")(Day11.answer2)

  test("Day 12: JSAbacusFramework.io"):
    assertResult(111754)(Day12.answer1)
    assertResult(65402)(Day12.answer2)

  test("Day 13: Knights of the Dinner Table"):
    assertResult(733)(Day13.answer1)
    assertResult(725)(Day13.answer2)

  test("Day 14: Reindeer Olympics"):
    assertResult(2660)(Day14.answer1)
    assertResult(1256)(Day14.answer2)

  test("Day 15: Science for Hungry People"):
    assertResult(13882464)(Day15.answer1)
    assertResult(11171160)(Day15.answer2)

  test("Day 16: Aunt Sue"):
    assertResult(40)(Day16.answer1)
    assertResult(241)(Day16.answer2)

  test("Day 17: No Such Thing as Too Much"):
    assertResult(4372)(Day17.answer1)
    assertResult(4)(Day17.answer2)

  test("Day 18: Like a GIF For Your Yard"):
    assertResult(814)(Day18.answer1)
    assertResult(924)(Day18.answer2)

  test("Day 19: Medicine for Rudolph"):
    assertResult(535)(Day19.answer1)
    assertResult(212)(Day19.answer2)

  test("Day 20: Infinite Elves and Infinite Houses"):
    assertResult(831600)(Day20.answer1)
    assertResult(884520)(Day20.answer2)

  test("Day 21: RPG Simulator 20XX"):
    assertResult(111)(Day21.answer1)
    assertResult(188)(Day21.answer2)

  test("Day 22: Wizard Simulator 20XX"):
    assertResult(1824)(Day22.answer1)
    assertResult(1937)(Day22.answer2)

  test("Day 23: Opening the Turing Lock"):
    assertResult(255)(Day23.answer1)
    assertResult(334)(Day23.answer2)

  test("Day 24: It Hangs in the Balance"):
    assertResult(11846773891L)(Day24.answer1)
    assertResult(80393059L)(Day24.answer2)

  test("Day 25: Let It Snow"):
    assertResult(19980801)(Day25.answer1)
