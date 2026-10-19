package aoc2018

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec
import scala.util.matching.*

object Day04 extends AoC:

  enum Event derives CanEqual:
    case BeginShift(guard: Int)
    case FallAsleep
    case WakeUp

  import Event.*

  type Timestamp = String

  case class Record(timestamp: Timestamp, event: Event):
    val Timestamp: Regex = """\d{4}-\d{2}-\d{2} \d{2}:(\d{2})""".r
    def minute: Int =
      timestamp match
        case Timestamp(minute) => minute.toInt

  object Record:
    val RecordLine: Regex = """\[(.{16})] (.*)""".r
    val GuardShift: Regex = """Guard #(\d+) begins shift""".r

    def fromLines(input: Vector[String]): Vector[Record] =
      input
        .map:
          case RecordLine(timestamp, event) =>
            Record(timestamp, event match
              case GuardShift(guard) => BeginShift(guard.toInt)
              case "falls asleep"    => FallAsleep
              case "wakes up"        => WakeUp
            )
        .sortBy(_.timestamp)

  case class Shift(guard: Int, sleep: Set[Int])

  def fromRecords(records: Vector[Record]): Vector[Shift] =
    @tailrec
    def loop(remaining: Vector[Record], completed: Vector[Shift], current: Option[Shift], sleepStarted: Option[Int]): Vector[Shift] =
      if remaining.isEmpty then
        completed ++ current.toVector
      else
        val record = remaining.head
        val rest   = remaining.tail

        record.event match
          case BeginShift(guard) =>
            loop(rest, completed ++ current.toVector, Some(Shift(guard, Set.empty)), None)
          case FallAsleep =>
            loop(rest, completed, current, Some(record.minute))
          case WakeUp =>
            val nap = (sleepStarted.get until record.minute).toSet
            loop(rest, completed, current.map(shift => shift.copy(sleep = shift.sleep ++ nap)), None)

    loop(records, Vector.empty, None, None)

  def solve1(records: Vector[Record]): Int =
    val shifts          = fromRecords(records)
    val guardSleeps     = shifts.groupMapReduce(_.guard)(_.sleep.toVector)(_ ++ _)
    val (guard, sleeps) = guardSleeps.maxBy((_, minutes) => minutes.length)
    val minute          = (0 until 60).maxBy(minute => sleeps.count(_ == minute))
    guard * minute

  def solve2(records: Vector[Record]): Int =
    val shifts              = fromRecords(records)
    val guardSleeps         = shifts.groupMapReduce(_.guard)(_.sleep.toVector)(_ ++ _)
    val guardMinuteCount    = guardSleeps.view.mapValues(_.countElements[Int]).toMap
    val minuteMaxGuardCount = (0 until 60).map(minute => minute -> guardMinuteCount.view.mapValues(_.getOrElse(minute, 0)).maxBy((_, count) => count)).toMap

    type GuardCount      = (Int, Int)
    type GuardCountEvent = (Int, (Int, Int))

    extension (guardCount: GuardCount)
      def guard: Int = guardCount._1
      def count: Int = guardCount._2

    extension (guardCountEvent: GuardCountEvent)
      def minute: Int            = guardCountEvent._1
      def guardCount: GuardCount = guardCountEvent._2

    val result = minuteMaxGuardCount.maxBy(_.guardCount.count)
    result.guardCount.guard * result.minute

  lazy val records: Vector[Record] = Record.fromLines(lines)

  override lazy val answer1: Int = solve1(records)
  override lazy val answer2: Int = solve2(records)
