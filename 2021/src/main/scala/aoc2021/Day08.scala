package aoc2021

import nmcb.*

import scala.annotation.tailrec

object Day08 extends AoC:

  type Encoding = (input: Vector[String], output: Vector[String])

  val encodings: Vector[Encoding] =
    lines.collect: line =>
      val input  = line.split("\\|")(0).trim.split(" ").toVector
      val output = line.split("\\|")(1).trim.split(" ").toVector
      (input = input, output = output)

  type Wiring = String

  def decode(wiring: Wiring)(digit: String): Option[Int] =

    val leds0 = Set(0, 1, 2, 4, 5, 6)
    val leds1 = Set(2, 5)
    val leds2 = Set(0, 2, 3, 4, 6)
    val leds3 = Set(0, 2, 3, 5, 6)
    val leds4 = Set(1, 2, 3, 5)
    val leds5 = Set(0, 1, 3, 5, 6)
    val leds6 = Set(0, 1, 3, 4, 5, 6)
    val leds7 = Set(0, 2, 5)
    val leds8 = Set(0, 1, 2, 3, 4, 5, 6)
    val leds9 = Set(0, 1, 2, 3, 5, 6)

    val leds = digit.map(code => wiring.indexOf(code)).toSet
    if      (leds == leds0) Some(0)
    else if (leds == leds1) Some(1)
    else if (leds == leds2) Some(2)
    else if (leds == leds3) Some(3)
    else if (leds == leds4) Some(4)
    else if (leds == leds5) Some(5)
    else if (leds == leds6) Some(6)
    else if (leds == leds7) Some(7)
    else if (leds == leds8) Some(8)
    else if (leds == leds9) Some(9)
    else None

  def valid(wiring: Wiring)(input: Vector[String]): Boolean =

    @tailrec
    def loop(todo: Vector[String], digitsFound: Set[Int] = Set.empty): Boolean =

      val allDigits: Set[Int] = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

      def inDigitsFound(digit: Int): Boolean =
        digitsFound.contains(digit)

      def notInDigitsLeftToValidate(digit: Int): Boolean =
        !(allDigits -- digitsFound).contains(digit)

      todo.runtimeChecked match
        case Vector() => digitsFound == allDigits
        case encoding +: encodings => decode(wiring)(encoding) match
          case None                                            => false
          case Some(digit) if inDigitsFound(digit)             => false
          case Some(digit) if notInDigitsLeftToValidate(digit) => false
          case Some(digit)                                     => loop(encodings, digitsFound + digit)

    loop(input)


  def solve1(encodings: Vector[Encoding]): Int =
    encodings
      .map: (_, output) =>
        output.count: digit =>
          val is1 = digit.length == 2
          val is7 = digit.length == 3
          val is4 = digit.length == 4
          val is8 = digit.length == 7
          is1 || is4 || is7 || is8
      .sum

  def solve2(encodings: Vector[Encoding]): Int =
    @tailrec
    def search(wirings: Vector[String], input: Vector[String]): String =
      wirings.runtimeChecked match
        case wiring +: rest if valid(wiring)(input) => wiring
        case _      +: rest                         => search(rest, input)
    val wirings = "abcdefg".permutations.toVector
    encodings
      .map: (input, output) =>
        val wiring: Wiring = search(wirings, input)
        output.foldLeft("")((num, digit) => num + decode(wiring)(digit).get).toInt
      .sum

  override lazy val answer1: Int = solve1(encodings)
  override lazy val answer2: Int = solve2(encodings)

