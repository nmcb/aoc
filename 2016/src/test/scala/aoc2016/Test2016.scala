package aoc2016

import org.scalatest.funsuite.AnyFunSuite

class Test2016 extends AnyFunSuite:

  test("Day 1: No Time for a Taxicab"):
    assertResult(288)(Day01.answer1)
    assertResult(111)(Day01.answer2)

  test("Day 2: Bathroom Security"):
    assertResult("38961")(Day02.answer1)
    assertResult("46C92")(Day02.answer2)

  test("Day 3: Squares With Three Sides"):
    assertResult( 869)(Day03.answer1)
    assertResult(1544)(Day03.answer2)

  test("Day 4: Security Through Obscurity"):
    assertResult(185371)(Day04.answer1)
    assertResult(   984)(Day04.answer2)

  test("Day 5: How About a Nice Game of Chess?"):
    assertResult("c6697b55")(Day05.answer1)
    assertResult("8c35d1ab")(Day05.answer2)

  test("Day 6: Signals and Noise"):
    assertResult("afwlyyyq")(Day06.answer1)
    assertResult("bhkzekao")(Day06.answer2)

  test("Day 7: Internet Protocol Version 7"):
    assertResult(118)(Day07.answer1)
    assertResult(260)(Day07.answer2)

  test("Day 8: Two-Factor Authentication"):
    assertResult(110)(Day08.answer1)
    assertResult("""####...##.#..#.###..#..#..##..###..#....#...#..##.
                   |...#....#.#..#.#..#.#.#..#..#.#..#.#....#...#...#.
                   |..#.....#.####.#..#.##...#....#..#.#.....#.#....#.
                   |.#......#.#..#.###..#.#..#....###..#......#.....#.
                   |#....#..#.#..#.#.#..#.#..#..#.#....#......#..#..#.
                   |####..##..#..#.#..#.#..#..##..#....####...#...##.."""
                   .stripMargin)(Day08.answer2)

  test("Day 9: Explosives in Cyberspace"):
    assertResult(112830)(Day09.answer1)
    assertResult(10931789799L)(Day09.answer2)

  test("Day 10: Balance Bots"):
    assertResult(98)(Day10.answer1)
    assertResult(4042)(Day10.answer2)

  test("Day 11: Radioisotope Thermoelectric Generators"):
    assertResult(47)(Day11.answer1)
    assertResult(71)(Day11.answer2)

  test("Day 12: Leonardo's Monorail"):
    assertResult(318007)(Day12.answer1)
    assertResult(9227661)(Day12.answer2)

  test("Day 13: A Maze of Twisty Little Cubicles"):
    assertResult(90)(Day13.answer1)
    assertResult(135)(Day13.answer2)

  test("Day 14: One-Time Pad"):
    assertResult(16106)(Day14.answer1)
    assertResult(22423)(Day14.answer2)

  test("Day 15: Timing is Everything"):
    assertResult(203660)(Day15.answer1)
    assertResult(2408135)(Day15.answer2)

  test("Day 16: Dragon Checksum"):
    assertResult("10101001010100001")(Day16.answer1)
    assertResult("10100001110101001")(Day16.answer2)

  test("Day 17: Two Steps Forward"):
    assertResult("DRDRULRDRD")(Day17.answer1)
    assertResult(384)(Day17.answer2)

  test("Day 18: Like a Rogue"):
    assertResult(1978)(Day18.answer1)
    assertResult(20003246)(Day18.answer2)

  test("Day 19: An Elephant Named Joseph"):
    assertResult(1841611)(Day19.answer1)
    assertResult(1423634)(Day19.answer2)

  test("Day 20: Firewall Rules"):
    assertResult(31053880)(Day20.answer1)
    assertResult(117)(Day20.answer2)

  test("Day 21: Scrambled Letters and Hash"):
    assertResult("dgfaehcb")(Day21.answer1)
    assertResult("fdhgacbe")(Day21.answer2)

  test("Day 22: Grid Computing"):
    assertResult(934)(Day22.answer1)
    assertResult(207)(Day22.answer2)

  test("Day 23: Safe Cracking"):
    assertResult(10152)(Day23.answer1)
    assertResult(479006712)(Day23.answer2)

  test("Day 24: Air Duct Spelunking"):
    assertResult(456)(Day24.answer1)
    assertResult(704)(Day24.answer2)

  test("Day 25: Clock Signal"):
    assertResult(196)(Day25.answer1)
