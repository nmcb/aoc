package aoc2015

import nmcb.*
import nmcb.predef.*

object Day14 extends AoC:

  case class Deer(name: String, velocity: Int, flyTime: Int, restTime: Int)

  object Deer:
    def fromString(s: String): Deer =
      s match
        case s"$name can fly $speed km/s for $fly seconds, but then must rest for $rest seconds." =>
          Deer(name, speed.toInt, fly.toInt, rest.toInt)

  enum Timer(val time: Int):
    case  FlyTime(override val time: Int) extends Timer(time)
    case RestTime(override val time: Int) extends Timer(time)

  import Timer.*
    
  case class Race(state: Vector[(Deer, Timer, Int)], scores: Map[Deer, Int]):
    
    lazy val maxDistance: Int =
      state.map((_, _, distance) => distance).max

    lazy val lead: Vector[Deer] =
      state.filter((_, _, distance) => distance == maxDistance).map((deer, _, _) => deer)

    lazy val maxScore: Int =
      scores.values.max

    def race: Race =
      copy(
        state = state.map: (deer, timer, distance) =>
          timer match
            case  FlyTime(t) if t < deer.flyTime  => ( deer,  FlyTime(t + 1), distance + deer.velocity )
            case  FlyTime(_)                      => ( deer, RestTime(    1), distance                 )
            case RestTime(t) if t < deer.restTime => ( deer, RestTime(t + 1), distance                 )
            case RestTime(_)                      => ( deer,  FlyTime(    1), distance + deer.velocity )
      )

    def withScore: Race =
      copy(
        scores = lead.foldLeft(scores): (scores, winning) =>
          scores.updatedWith(winning):
            case Some(s) => Some(s + 1)
            case None    => Some(    1)
      )

  object Race:

    def start(deer: Vector[Deer]): Race =
      Race(
        state    = deer.map(d => (d, FlyTime(0), 0)),
        scores   = Map.empty
      )

  val deer: Vector[Deer] = lines.map(Deer.fromString)

  override lazy val answer1: Int = Iterator.iterate(Race.start(deer))(_.race).nth(2503).maxDistance
  override lazy val answer2: Int = Iterator.iterate(Race.start(deer))(_.race.withScore).nth(2503).maxScore
