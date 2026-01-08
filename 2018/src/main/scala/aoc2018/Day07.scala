package aoc2018

import nmcb.*
import nmcb.predef.*

import scala.annotation.*
import scala.collection.immutable.SortedMap

object Day07 extends AoC:

  def solve[A](edges: Vector[(A,A)], timer: A => Int, parallelization: Int)(using Ordering[A]): (Int,Vector[A]) =

    case class Work(queue: SortedMap[A,Int] = SortedMap(), workers: SortedMap[A,Int] = SortedMap()):

      def isDone: Boolean =
        queue.isEmpty && workers.isEmpty

      def tick(schedule: Vector[A]): (Vector[A], Work) =
        val work = queue ++ schedule.map(a => a -> timer(a))
        val (add, left)  = work.splitAt(parallelization - workers.size)
        val (done, todo) = (workers ++ add).map((c,t) => (c, t - 1)).partition((_,t) => t == 0)
        (done.keys.toVector, Work(queue = left, workers = todo))

    @tailrec
    def loop(dependencies: SortedMap[A,Set[A]], time: Int = 0, work: Work = Work(), done: Vector[A] = Vector()): (Int, Vector[A]) =
      if dependencies.isEmpty && work.isDone then
        (time, done)
      else
        val (nodeps, hasdeps) = dependencies.partition((_,ds) => ds.isEmpty)
        val (processed, working) = work.tick(nodeps.keys.toVector)
        val next = hasdeps.map((node,ds) => node -> (ds -- processed))
        loop(next, time + 1, working, done :++ processed)

    loop(edges.foldLeft(SortedMap[A,Set[A]]())((ds,e) =>
      ds + (e.left -> ds.getOrElse(e.left, Set())) + (e.right -> (ds.getOrElse(e.right, Set()) + e.left))
    ))

  val steps: Vector[(Char,Char)] =
    def parser(s: String): (Char,Char) =
      s match
        case s"Step $fst must be finished before step $snd can begin." => (fst.head, snd.head)
        case _ => sys.error("boom!")
    lines.map(parser)

  lazy val answer1: String = solve(steps, timer = _ => 1, parallelization = 1).right.mkString("")
  lazy val (answer2, _)    = solve(steps, timer = _.toInt - 4, parallelization = 5)
