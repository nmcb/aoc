import aoc2018.{Day01, Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25}
import org.scalatest.funsuite.AnyFunSuite

class Test2018 extends AnyFunSuite:

  test("Day 1: Chronal Calibration"):
    assertResult(477)(Day01.answer1)
    assertResult(390)(Day01.answer2)

  test("Day 2: Inventory Management System"):
    assertResult(5368)(Day02.answer1)
    assertResult("cvgywxqubnuaefmsldjrpfzyi")(Day02.answer2)

  test("Day 3: No Matter How You Slice It"):
    assertResult(107043)(Day03.answer1)
    assertResult(346)(Day03.answer2)

  test("Day 4: Repose Record"):
    assertResult(103720)(Day04.answer1)
    assertResult(110913)(Day04.answer2)

  test("Day 5: Alchemical Reduction"):
    assertResult(11310)(Day05.answer1)
    assertResult(6020)(Day05.answer2)

  test("Day 6: Chronal Coordinates"):
    assertResult(2906)(Day06.answer1)
    assertResult(50530)(Day06.answer2)

  test("Day 7: The Sum of Its Parts"):
    assertResult("FHICMRTXYDBOAJNPWQGVZUEKLS")(Day07.answer1)
    assertResult(946)(Day07.answer2)

  test("Day 8: Memory Maneuver"):
    assertResult(47244)(Day08.answer1)
    assertResult(17267)(Day08.answer2)

  test("Day 9: Marble Mania"):
    assertResult(412117)(Day09.answer1)
    assertResult(3444129546L)(Day09.answer2)

  test("Day 10: The Stars Align"):
    assertResult(
      """######..#####....####...#....#..#.........##.......###..#.....
        |#.......#....#..#....#..#....#..#........#..#.......#...#.....
        |#.......#....#..#........#..#...#.......#....#......#...#.....
        |#.......#....#..#........#..#...#.......#....#......#...#.....
        |#####...#####...#.........##....#.......#....#......#...#.....
        |#.......#..#....#.........##....#.......######......#...#.....
        |#.......#...#...#........#..#...#.......#....#......#...#.....
        |#.......#...#...#........#..#...#.......#....#..#...#...#.....
        |#.......#....#..#....#..#....#..#.......#....#..#...#...#.....
        |######..#....#...####...#....#..######..#....#...###....######
        |""".stripMargin)(Day10.sky.asString)
    assertResult(10813)(Day10.answer2)

  test("Day 11: Chronal Charge"):
    assertResult("21,37")(Day11.answer1)
    assertResult("236,146,12")(Day11.answer2)

  test("Day 12: Subterranean Sustainability"):
    assertResult(2909)(Day12.answer1)
    assertResult(2500000001175L)(Day12.answer2)

  test("Day 13: Mine Cart Madness"):
    assertResult("41,22")(Day13.answer1)
    assertResult("84,90")(Day13.answer2)

  test("Day 14: Chocolate Charts"):
    assertResult(1150511382)(Day14.answer1)
    assertResult(20173656)(Day14.answer2)

  test("Day 15: Beverage Bandits"):
    assertResult(250648)(Day15.answer1)
    assertResult(42224)(Day15.answer2)

  test("Day 16: Chronal Classification"):
    assertResult(640)(Day16.answer1)
    assertResult(472)(Day16.answer2)

  test("Day 17: Reservoir Research"):
    assertResult(34379)(Day17.answer1)
    assertResult(28015)(Day17.answer2)

  test("Day 18: Settlers of The North Pole"):
    assertResult(637550)(Day18.answer1)
    assertResult(201465)(Day18.answer2)

  test("Day 19: Go With The Flow"):
    assertResult(978)(Day19.answer1)
    assertResult(10996992)(Day19.answer2)

  test("Day 20: A Regular Map"):
    assertResult(3721)(Day20.answer1)
    assertResult(8613)(Day20.answer2)

  test("Day 21: Chronal Conversion"):
    assertResult(1024276)(Day21.answer1)
    assertResult(5876609)(Day21.answer2)

  test("Day 22: Mode Maze"):
    assertResult(7402)(Day22.answer1)
    assertResult(1025)(Day22.answer2)

  test("Day 23: Experimental Emergency Teleportation"):
    assertResult(309)(Day23.answer1)
    assertResult(119011326)(Day23.answer2)

  test("Day 24: Immune System Simulator 20XX"):
    assertResult(20340)(Day24.answer1)
    assertResult(3862)(Day24.answer2)

  test("Day 25: Four-Dimensional Adventure"):
    assertResult(324)(Day25.answer1)
