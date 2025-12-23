import aoc2017.{Day01, Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25}
import org.scalatest.funsuite.AnyFunSuite

class Test2017 extends AnyFunSuite:
  
  test("Day 1: Inverse Captcha"):
    assertResult(1069)(Day01.answer1)
    assertResult(1268)(Day01.answer2)

  test("Day 2: Corruption Checksum"):
    assertResult(45158)(Day02.answer1)
    assertResult(294)(Day02.answer2)

  test("Day 3: Spiral Memory"):
    assertResult(475)(Day03.answer1)
    assertResult(279138)(Day03.answer2)

  test("Day 4: High-Entropy Passphrases"):
    assertResult(337)(Day04.answer1)
    assertResult(231)(Day04.answer2)

  test("Day 5: A Maze of Twisty Trampolines, All Alike"):
    assertResult(372671)(Day05.answer1)
    assertResult(25608480)(Day05.answer2)

  test("Day 6: Memory Reallocation"):
    assertResult(12841)(Day06.answer1)
    assertResult(8038)(Day06.answer2)

  test("Day 7: Recursive Circus"):
    assertResult("eqgvf")(Day07.answer1)
    assertResult(757)(Day07.answer2)

  test("Day 8: I Heard You Like Registers"):
    assertResult(4416)(Day08.answer1)
    assertResult(5199)(Day08.answer2)

  test("Day 9: Stream Processing"):
    assertResult(8337)(Day09.answer1)
    assertResult(4330)(Day09.answer2)

  test("Day 10: Knot Hash"):
    assertResult(15990)(Day10.answer1)
    assertResult("90adb097dd55dea8305c900372258ac6", Day10.answer2)

  test("Day 11: Hex Ed"):
    assertResult(670)(Day11.answer1)
    assertResult(1426)(Day11.answer2)

  test("Day 12: Digital Plumber"):
    assertResult(113)(Day12.answer1)
    assertResult(202)(Day12.answer2)

  test("Day 13: Packet Scanners"):
    assertResult(1300)(Day13.answer1)
    assertResult(3870382)(Day13.answer2)

  test("Day 14: Disk Defragmentation"):
    assertResult(8194)(Day14.answer1)
    assertResult(1141)(Day14.answer2)

  test("Day 15: Dueling Generators"):
    assertResult(619L)(Day15.answer1)
    assertResult(290L)(Day15.answer2)

  test("Day 16: Permutation Promenade"):
    assertResult("lgpkniodmjacfbeh")(Day16.answer1)
    assertResult("hklecbpnjigoafmd")(Day16.answer2)

  test("Day 17: Spinlock"):
    assertResult(1244)(Day17.answer1)
    assertResult(11162912)(Day17.answer2)

  test("Day 18: Duet"):
    assertResult(1187)(Day18.answer1)
    assertResult(5969)(Day18.answer2)

  test("Day 19: A Series of Tubes"):
    assertResult("DWNBGECOMY")(Day19.answer1)
    assertResult(17228)(Day19.answer2)

  test("Day 20: Particle Swarm"):
    assertResult(376)(Day20.answer1)
    assertResult(574)(Day20.answer2)

  test("Day 21: Fractal Art"):
    assertResult(162)(Day21.answer1)
    assertResult(2264586)(Day21.answer2)

  test("Day 22: Sporifica Virus"):
    assertResult(5256)(Day22.answer1)
    assertResult(2511345)(Day22.answer2)

  test("Day 23: Coprocessor Conflagration"):
    assertResult(3025)(Day23.answer1)
    assertResult(915)(Day23.answer2)

  test("Day 24: Electromagnetic Moat"):
    assertResult(1511)(Day24.answer1)
    assertResult(1471)(Day24.answer2)

  test("Day 25: The Halting Problem"):
    assertResult(4230)(Day25.answer1)
