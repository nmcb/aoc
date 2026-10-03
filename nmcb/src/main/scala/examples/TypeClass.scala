package examples

import scala.annotation.tailrec

object TypeClass:

  trait Order[A]:
    def gt(l: A, r: A): Boolean

  def sort[A : Order](list: List[A]): List[A] =

    @tailrec
    def bubble(list: List[A], n: Int): List[A] =
      if n == list.length - 1 then
        list
      else
        val l = list(n)
        val r = list(n + 1)
        if summon[Order[A]].gt(l, r) then
          val swapped = list.slice(0, n) :++ List(r) :++ List(l) :++ list.slice(n + 2, list.length)
          val next = n - 1
          bubble(swapped, if next <= 0 then 0 else next)
        else
          bubble(list, n + 1)

    bubble(list, 0)

  given Order[Int] =
    (l: Int, r: Int) => l > r

  private val data1: List[Int] =
    List(1, 9, 2, 8, 3, 7, 4, 6, 5, 1)

  case class Person(name: String, age: Int)

  private object Person:

    given Order[Person] =
      (l: Person, r: Person) => l.age > r.age

  private val data2: List[Person] =
    List(Person("Marco", 53), Person("Kristien", 23), Person("Magnus", 19))

  val nameLengthOrder: Order[Person] =
    (l: Person, r: Person) => l.name.length > r.name.length

  @main def run(): Unit =
    println(s"data1=$data1")
    println(s"sort1=${sort(data1)}")
    println(s"data2=$data2")
    println(s"sort2 - default=${sort(data2)}")
    println(s"sort2 - nameLength=${sort(data2)(using nameLengthOrder)}")




