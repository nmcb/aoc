package aoc2019

import org.scalatest.funsuite.AnyFunSuite

class Test2019 extends AnyFunSuite:

  test("Day 1: The Tyranny of the Rocket Equation"):
    assertResult(3331849)(Day01.answer1)
    assertResult(4994530)(Day01.answer2)

  test("Day 2: 1202 Program Alarm"):
    assertResult(4576384)(Day02.answer1)
    assertResult(5398)(Day02.answer2)

  test("Day 3: Crossed Wires"):
    assertResult(308)(Day03.answer1)
    assertResult(12934)(Day03.answer2)

  test("Day 4: Secure Container"):
    assertResult(1178)(Day04.answer1)
    assertResult(763)(Day04.answer2)

  test("Day 5: Sunny with a Chance of Asteroids"):
    assertResult(15386262)(Day05.answer1)
    assertResult(10376124)(Day05.answer2)

  test("Day 6: Universal Orbit Map"):
    assertResult(162439)(Day06.answer1)
    assertResult(368)(Day06.answer2)

  test("Day 7: Amplification Circuit"):
    assertResult(21760)(Day07.answer1)
    assertResult(69816958)(Day07.answer2)

  test("Day 8: Space Image Format"):
    assertResult(1920)(Day08.answer1)
    assertResult(
      """███░░░██░░█░░█░█░░░░░██░░
        |█░░█░█░░█░█░░█░█░░░░█░░█░
        |█░░█░█░░░░█░░█░█░░░░█░░█░
        |███░░█░░░░█░░█░█░░░░████░
        |█░░░░█░░█░█░░█░█░░░░█░░█░
        |█░░░░░██░░░██░░████░█░░█░
        |""".stripMargin)(Day08.answer2)

  test("Day 9: Sensor Boost"):
    assertResult(2399197539L)(Day09.answer1)
    assertResult(35106)(Day09.answer2)

  test("Day 10: Monitoring Station"):
    assertResult(214)(Day10.answer1)
    assertResult(502)(Day10.answer2)

  test("Day 11: Space Police"):
    assertResult(1686)(Day11.answer1)
    assertResult(
      """░░██░░░██░░███░░███░░█░░█░████░█░░█░█░░░░░░
        |░█░░█░█░░█░█░░█░█░░█░█░█░░░░░█░█░░█░█░░░░░░
        |░█░░░░█░░█░█░░█░█░░█░██░░░░░█░░█░░█░█░░░░░░
        |░█░██░████░███░░███░░█░█░░░█░░░█░░█░█░░░░░░
        |░█░░█░█░░█░█░█░░█░░░░█░█░░█░░░░█░░█░█░░░░░░
        |░░███░█░░█░█░░█░█░░░░█░░█░████░░██░░████░░░
        |""".stripMargin)(Day11.answer2)

  test("Day 12: The N-Body Problem"):
    assertResult(13045)(Day12.answer1)
    assertResult(344724687853944L)(Day12.answer2)

  test("Day 13: Care Package"):
    assertResult(361)(Day13.answer1)
    assertResult(17590)(Day13.answer2)

  test("Day 14: Space Stoichiometry"):
    assertResult(143173)(Day14.answer1)
    assertResult(8845261)(Day14.answer2)

  test("Day 15: Oxygen System"):
    assertResult(270)(Day15.answer1)
    assertResult(364)(Day15.answer2)

  test("Day 16: Flawed Frequency Transmission"):
    assertResult("27229269")(Day16.answer1)
    assertResult("26857164")(Day16.answer2)

  test("Day 17: Set and Forget"):
    assertResult(6448)(Day17.answer1)
    assertResult(914900)(Day17.answer2)

  test("Day 18: Many-Worlds Interpretation"):
    assertResult(3270)(Day18.answer1)
    assertResult(1628)(Day18.answer2)

  test("Day 19: Tractor Beam"):
    assertResult(147)(Day19.answer1)
    assertResult(13280865)(Day19.answer2)

  test("Day 20: Donut Maze"):
    assertResult(644)(Day20.answer1)
    assertResult(7798)(Day20.answer2)

  test("Day 21: Springdroid Adventure"):
    assertResult(19354392)(Day21.answer1)
    assertResult(1139528802)(Day21.answer2)

  test("Day 22: Slam Shuffle"):
    assertResult(7171)(Day22.answer1)
    assertResult(73394009116480L)(Day22.answer2)

  test("Day 23: Category Six"):
    assertResult(19473)(Day23.answer1)
    assertResult(12475)(Day23.answer2)

  test("Day 24: Planet of Discord"):
    assertResult(18400821)(Day24.answer1)
    assertResult(1914)(Day24.answer2)

  test("Day 25: Cryostasis"):
    assertResult(2236672)(Day25.answer1)
