package aoc2019

import nmcb.*
import nmcb.pos.{*, given}

import scala.annotation.tailrec

object Day10 extends AoC:

  extension (p: Pos)
    def part2: Int = p.x * 100 + p.y

  val astroids: List[Pos] =
    val sizeX = lines(0).length
    val sizeY = lines.length
    List
      .tabulate(sizeX, sizeY)((x,y) => if lines(y)(x) == '#' then Some((x,y)) else None)
      .flatten
      .flatten

  def blockedBy(astroids: List[Pos], a: Pos, o: Pos): Boolean =

    @tailrec
    def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    val x = o.x - a.x
    val y = o.y - a.y
    val d = gcd(math.abs(x), math.abs(y))
    val nx = x / d
    val ny = y / d
    (1 until d).exists(m => astroids.contains((a.x + (m * nx), a.y + (m * ny))))

  def maxBlockedByCount(astroids: List[Pos]): Int =

      def blockCount(astroid: Pos): Int =
        astroids
          .filterNot(other => other != astroid && blockedBy(astroids, astroid,other))
          .length

      astroids
        .groupMapReduce(identity)(blockCount)(_ max _)
        .values
        .max - 1

  type Aim = (Double,Double,Pos)

  extension (aim: Aim)
    def angle: Double    = aim._1
    def distance: Double = aim._2
    def target: Pos      = aim._3

  def testLaser(astroid: Pos)(astroids: List[Pos]): List[Aim] =

    import Ordering.Double.TotalOrdering
    astroids
      .filterNot(_ == astroid)
      .map(other => (astroid.angleDegrees(other), astroid.pythagoreanDistance(other), other))
      .sortBy(s => (s.angle, s.distance, s.target.x, s.target.y))

  @tailrec
  def fireAll(nr: Int)(todo: List[Aim], done: List[Aim] = List.empty, count: Int = 1, result: Option[Pos] = None): Option[Pos] =
    if todo.isEmpty && done.nonEmpty then
      fireAll(nr)(done, List.empty, count, result)
    else if todo.isEmpty then
      result
    else
      val aim   = todo.head
      val rest  = todo.tail
      val left  = rest.takeWhile(a => a.angle == aim.angle)
      val right = rest.dropWhile(a => a.angle == aim.angle)
      fireAll(nr)(right, done ++ left, count + 1, Option.when (count == nr)(aim.target).orElse(result))

  override lazy val answer1: Int = maxBlockedByCount(astroids)
  override lazy val answer2: Int = fireAll(200)(testLaser((8,16))(astroids)).get.part2
