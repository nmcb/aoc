package aoc2018

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day24 extends AoC:

  enum Army:
    case Draw
    case Immune
    case Infection

  import Army.*

  type AttackKind = String

  case class Attribute(hitPower: Int, initiative: Int)
  case class Defense(immune: Set[AttackKind], weak: Set[AttackKind])
  case class Attack(damage: Int, kind: AttackKind)

  case class Group(army: Army, attribute: Attribute, defense: Defense, attack: Attack)(var units: Int):

    def effectivePower: Int =
      units * attack.damage

    def computeDamage(attacker: Group): Int =
      if defense.immune.contains(attacker.attack.kind) then
        0
      else if defense.weak.contains(attacker.attack.kind) then
        2 * attacker.effectivePower
      else
        attacker.effectivePower

  def parseArmies(chunks: Vector[Vector[String]])(boost: Int): Vector[Group] =

    def parseGroup(army: Army, boost: Int)(line: String): Group =
      val GroupLine  = """(\d+) units each with (\d+) hit points .*with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
      val ImmuneLine = """immune to (.+?)[;|)]""".r.unanchored
      val WeakLine   = """weak to (.+?)[;|)]""".r.unanchored

      val immune: Set[String] = line match
        case ImmuneLine(items) => items.split(", ").toSet
        case _                 => Set.empty

      val weak: Set[String] = line match
        case WeakLine(items) => items.split(", ").toSet
        case _               => Set.empty

      line match
        case GroupLine(units, hitPower, damage, kind, initiative) =>
          val attribute = Attribute(hitPower.toInt, initiative.toInt)
          val defense   = Defense(immune, weak)
          val attack    = Attack(damage.toInt + boost, kind)
          Group(army, attribute, defense, attack)(units.toInt)


    val immune    = chunks(0).tail.map(parseGroup(Immune, boost))
    val infection = chunks(1).tail.map(parseGroup(Infection, 0))
    immune ++ infection

  type Outcome = (Army, Int)

  extension (outcome: Outcome)
    def winner: Army = outcome._1
    def units: Int   = outcome._2


  extension (groups: Vector[Group])
    @tailrec
    def fight: Outcome =

      val previous: Int =
        groups.map(_.units).sum

      val targetSelection: Vector[Group] =
        groups.sortBy(group => (-group.effectivePower, -group.attribute.initiative))

      val targets: Map[Group,Group] =
        targetSelection.foldLeft(Map.empty[Group, Group]): (targets,next) =>
          val candidate = groups
            .filterNot(targets.contains)
            .filter(_.army != next.army)
            .filter(_.units > 0)
            .filter(_.computeDamage(next) > 0)
            .maxByOption(target => (target.computeDamage(next), target.effectivePower, target.attribute.initiative))

          candidate match
            case Some(target) => targets.updated(target, next)
            case None         => targets

      targets
        .toVector
        .sortBy: (target, attacker) =>
          -attacker.attribute.initiative
        .foreach: (target, attacker) =>
          target.units = (target.units - target.computeDamage(attacker) / target.attribute.hitPower).max(0)

      if groups.map(_.units).sum == previous then
        (Draw, -1)
      else if groups.filter(_.units > 0).map(_.army).toSet.size == 1 then
        (groups.filter(_.units > 0).head.army, groups.map(_.units).sum)
      else
        groups.fight

  override lazy val answer1: Int = parseArmies(chunks)(boost = 0).fight.units
  override lazy val answer2: Int = Iterator.from(1).map(parseArmies(chunks)).map(_.fight).findFirst(_.winner == Immune).units
