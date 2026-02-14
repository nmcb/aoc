package aoc2018

import nmcb.*
import nmcb.predef.*

import scala.annotation.*
import scala.collection.immutable.SortedMap

object Day07 extends AoC:

  def solve[A](edges: Vector[(A, A)], timer: A => Int, parallelization: Int)(using Ordering[A]): (Int, Vector[A]) =

    case class Work(queue: SortedMap[A, Int] = SortedMap(), workers: SortedMap[A, Int] = SortedMap()):

      def isDone: Boolean =
        queue.isEmpty && workers.isEmpty

      def tick(schedule: Vector[A]): (Vector[A], Work) =
        val work         = queue ++ schedule.map(a => a -> timer(a))
        val (add, left)  = work.splitAt(parallelization - workers.size)
        val (done, todo) = (workers ++ add).map((c,t) => (c, t - 1)).partition((_,t) => t == 0)
        (done.keys.toVector, Work(queue = left, workers = todo))

    @tailrec
    def loop(dependencies: SortedMap[A, Set[A]], time: Int = 0, work: Work = Work(), done: Vector[A] = Vector()): (Int, Vector[A]) =
      if dependencies.isEmpty && work.isDone then
        (time, done)
      else
        val (leaves, branches)   = dependencies.partition((_,ds) => ds.isEmpty)
        val (processed, working) = work.tick(leaves.keys.toVector)
        val next                 = branches.map((node,ds) => node -> (ds -- processed))
        loop(next, time + 1, working, done :++ processed)

    val dependencies: SortedMap[A, Set[A]] =
      edges.
        foldLeft(SortedMap[A, Set[A]]()): (ds, e) =>
          val (a, b) = e
          ds + (a -> ds.getOrElse(a, Set())) + (b -> (ds.getOrElse(b, Set()) + a))

    loop(dependencies)

  val steps: Vector[(Char,Char)] =
    def parser(s: String): (Char,Char) =
      s.runtimeChecked match
        case s"Step $first must be finished before step $second can begin." => (first.head, second.head)
    lines.map(parser)

  override lazy val answer1: String = solve(steps, timer = _ => 1, parallelization = 1).right.mkString("")
  override lazy val answer2: Int    = solve(steps, timer = _.toInt - 4, parallelization = 5).left
