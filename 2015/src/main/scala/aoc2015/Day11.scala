package aoc2015

import nmcb.*

import scala.annotation.*

object Day11 extends AoC:

  case class Dig(value: Char = 'a'):
    import Dig.*
    import Carry.*

    override def toString: String =
      value.toString

    def increment: Option[Dig] =
      val (n, c) = increment(Zero)
      if c == Zero then Some(n) else None

    def increment(carry: Carry = Zero): (Dig, Carry) =
      val added = value + One.value + carry.value
      val nextDigit = if added <= MaxValue then copy(value = added.toChar) else copy(value = (added - DigRange).toChar)
      val nextCarry = if added <= MaxValue then Zero else One
      (nextDigit, nextCarry)

  object Dig:

    enum Carry(val value: Int) derives CanEqual:
      case Zero extends Carry(0x00)
      case One  extends Carry(value = 0x01)

    val MinValue: Char = 'a'
    val MaxValue: Char = 'z'
    val DigRange: Char = (MaxValue - MinValue + 1).toChar

    extension (c: Char)
      
      def toDig: Dig =
        Dig(c)


  case class Password(number: Array[Dig]):
    import Dig.*
    import Carry.*
    import Password.*

    override def toString: String =
      number.mkString

    @tailrec
    final def next: Password =
      @tailrec
      def increment(todo: Array[Dig], carry: Carry, acc: Array[Dig]): (Array[Dig], Carry) =
        if todo.isEmpty then
          (acc, carry)
        else
          val (n, c) = todo.last.increment(carry)
          if c == Zero then
            (todo.init ++: n +: acc, Zero)
          else
            increment(todo.init, Zero, n +: acc)

      val (n, c) = increment(number, Zero, Array.empty)
      val result = if c == Zero then Password(n) else Password.empty
      if result.valid then result else result.next

    def includesIncreasingStraight: Boolean =
      number
        .sliding(3)
        .exists(ds => ds(0).increment.contains(ds(1)) && ds(1).increment.contains(ds(2)))

    def includesLegalCharacters: Boolean =
      number.forall(d => d.value != 'i' && d.value != 'o' && d.value != 'l')

    def includesNonOverlappingPair: Boolean =
      @tailrec
      def hasNonOverlappingPair(todo: String, foundFirst: Boolean = false): Boolean =
        todo.toList match
          case c0 :: c1 :: _ if c0 == c1 && foundFirst => true
          case c0 :: c1 :: t if c0 == c1               => hasNonOverlappingPair(t.mkString, true)
          case  _ :: rest                              => hasNonOverlappingPair(rest.mkString, foundFirst)
          case Nil                                     => false
      hasNonOverlappingPair(number.show)

    def valid: Boolean =
      includesIncreasingStraight && includesLegalCharacters && includesNonOverlappingPair

  object Password:

    import Dig.*

    def empty: Password =
      Password.fromString("aaaaaaaa")

    def fromString(string: String): Password =
      Password(string.foldLeft(Array.empty[Dig])(_ :+ _.toDig))

    extension (number: Array[Dig])
      def show: String =
        number.mkString


  override lazy val input: String = "hxbxwxba"

  override lazy val answer1: String = Password.fromString(input).next.toString
  override lazy val answer2: String = Password.fromString(answer1).next.toString
