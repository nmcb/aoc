package aoc2015

import nmcb.*
import scala.annotation.*

object Day08 extends AoC:

  extension (quoted: String)

    def unquoted: String =
      quoted.drop(1).dropRight(1)

    def unescaped: String =
      @tailrec
      def loop(todo: List[Char], ret: String = ""): String =
        todo match
          case Nil =>
            ret
          case '\\' :: '\\' :: rest =>
            loop(rest, ret + '\\')
          case '\\' :: '\"' :: rest =>
            loop(rest, ret + '\"')
          case '\\' ::  'x' :: h1 :: h2 :: rest =>
            loop(rest, ret + Integer.parseInt(s"$h1$h2", 16).toChar)
          case c1 :: rest =>
            loop(rest, ret + c1)

      loop(unquoted.toList)

    def escaped: String =
      @tailrec
      def loop(todo: List[Char], ret: String = ""): String =
        todo match
          case Nil          => "\"" + ret + "\""
          case '\"' :: rest => loop(rest, ret + "\\\"")
          case '\\' :: rest => loop(rest, ret + "\\\\")
          case   c1 :: rest => loop(rest, ret + c1)

      loop(quoted.toList)

  lazy val answer1: Int = lines.map(str => str.length - str.unescaped.length).sum
  lazy val answer2: Int = lines.map(str => str.escaped.length - str.length).sum
