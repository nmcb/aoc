package aoc2015

import nmcb.*

object Day13 extends AoC:

  type Preferences = Map[String, Map[String, Int]]

  case class Table(setting: Vector[String], prefs: Preferences):

    val neighbours: Map[String, Vector[String]] =
      val n1 = Vector(setting.init.last, setting.last, setting.head)
      val n2 = Vector(setting.last, setting.head, setting.tail.head)
      (n1 +: n2 +: setting.sliding(3).toVector)
        .collect:
          case Vector(nl, name, nr) => name -> Vector(nl, nr)
        .toMap

    def happiness(name: String): Int =
      val Vector(n1, n2) = neighbours(name)
      val h1 = prefs(name)(n1)
      val h2 = prefs(name)(n2)
      h1 + h2

    def totalHappiness: Int =
      setting.foldLeft(0)((h,n) => h + happiness(n))

  val preferences1: Preferences =
    lines
      .collect:
        case s"$name would gain $value happiness units by sitting next to $neighbour." =>
          (name, neighbour, value.toInt)
        case s"$name would lose $value happiness units by sitting next to $neighbour." =>
          (name, neighbour, -value.toInt)
      .foldLeft[Preferences](Map.empty):
        case (ps, (name, neighbour, gain)) =>
          val happiness = neighbour -> gain
          ps.updatedWith(name)(_.map(_ + happiness).orElse(Some(Map(happiness))))

  val names1: Vector[String] =
    preferences1.keys.toVector

  val name: String =
    "Marco"
    
  val names2: Vector[String] =
    name +: names1

  val preferences2: Preferences =
    preferences1.map((n, p) => n -> (p + (name -> 0))) + (name -> preferences1.keys.map(n => n -> 0).toMap)

  override lazy val answer1: Int = names1.permutations.map(setting => Table(setting, preferences1).totalHappiness).max
  override lazy val answer2: Int = names2.permutations.map(setting => Table(setting, preferences2).totalHappiness).max
