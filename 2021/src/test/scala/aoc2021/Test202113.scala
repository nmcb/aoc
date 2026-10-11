package aoc2021

import org.scalatest.funsuite.AnyFunSuite

class Test202113 extends AnyFunSuite:
  
  test("2021 Day 13: Transparent Origami"):
    assertResult(763)(Day13.answer1)
    assertResult(
      """
        |###..#..#..##..#....###...##..###...##.
        |#..#.#..#.#..#.#....#..#.#..#.#..#.#..#
        |#..#.####.#..#.#....#..#.#....#..#.#..#
        |###..#..#.####.#....###..#....###..####
        |#.#..#..#.#..#.#....#.#..#..#.#.#..#..#
        |#..#.#..#.#..#.####.#..#..##..#..#.#..#
        |""".stripMargin)(Day13.answer2)
