package examples

import scala.annotation.tailrec

object TypeClass:

  trait Order[A]:
    def gt(l: A, r: A): Boolean

  extension [A : Order](sequence: IndexedSeq[A])

    def sort: IndexedSeq[A] =

      @tailrec
      def bubble(seq: IndexedSeq[A], n: Int): IndexedSeq[A] =
        if n == seq.length - 1 then
          seq
        else
          val l = seq(n)
          val r = seq(n + 1)
          if summon[Order[A]].gt(l, r) then
            val swapped = seq.slice(0, n) :++ List(r) :++ List(l) :++ seq.slice(n + 2, seq.length)
            val next = n - 1
            bubble(swapped, if next <= 0 then 0 else next)
          else
            bubble(seq, n + 1)

      bubble(sequence, 0)

  given Order[Int] =
    (l: Int, r: Int) => l > r

  private val data1: Vector[Int] =
    Vector(1, 9, 2, 8, 3, 7, 4, 6, 5, 1)

  case class Person(name: String, age: Int)

  private object Person:

    given Order[Person] =
      (l: Person, r: Person) => l.age > r.age

  private val data2: Vector[Person] =
    Vector(Person("Marco", 53), Person("Kristien", 23), Person("Magnus", 19))

  val nameLengthOrder: Order[Person] =
    (l: Person, r: Person) => l.name.length > r.name.length

  @main def run(): Unit =
    println(s"data1 = $data1")
    println(s"sort1 = ${data1.sort}")
    println(s"data2 = $data2")
    println(s"sort2 - default         = ${data2.sort}")
    println(s"sort2 - nameLengthOrder = ${data2.sort(using nameLengthOrder)}")
