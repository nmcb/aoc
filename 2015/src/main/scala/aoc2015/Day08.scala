package aoc2015

import nmcb.*

import java.lang.Integer.parseInt
import scala.annotation.*

object Day08 extends AoC:

  extension (quoted: String)

    def unquote: String =
      quoted match
        case s"\"$unquoted\"" => unquoted
        case _                => quoted

    def unescaped: String =
      @tailrec
      def loop(todo: Vector[Char], result: String = ""): String =
        todo.runtimeChecked match
          case Vector()                         => result
          case '\\' +: '\\' +: rest             => loop(rest, result + '\\')
          case '\\' +: '\"' +: rest             => loop(rest, result + '\"')
          case '\\' +:  'x' +: h1 +: h2 +: rest => loop(rest, result + parseInt(s"$h1$h2", 16).toChar)
          case c1 +: rest                       => loop(rest, result + c1)
      loop(unquote.toVector)

    def escaped: String =
      @tailrec
      def loop(todo: Vector[Char], result: String = ""): String =
        todo.runtimeChecked match
          case Vector()     => "\"" + result + "\""
          case '\"' +: rest => loop(rest, result + "\\\"")
          case '\\' +: rest => loop(rest, result + "\\\\")
          case   c1 +: rest => loop(rest, result + c1)
      loop(quoted.toVector)

  override lazy val answer1: Int = lines.map(line => line.length - line.unescaped.length).sum
  override lazy val answer2: Int = lines.map(line => line.escaped.length - line.length).sum
