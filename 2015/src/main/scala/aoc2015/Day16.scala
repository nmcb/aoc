package aoc2015

import nmcb.*

object Day16 extends AoC:

  case class Sue(nr: Int, properties: Map[String,Int]):
    def matches1: Boolean =
      properties.forall((name, count) =>
        Found.compounds.get(name).contains(count))

    def matches2: Boolean =
      properties.forall((name, count) =>
        name match
          case "cats" | "trees" =>
            Found.compounds.get(name).exists(_ < count)
          case "pomeranians" | "goldfish" =>
            Found.compounds.get(name).exists(_ > count)
          case _ =>
            Found.compounds.get(name).contains(count))

  object Sue:
    def fromString(s: String): Sue =
      s match
        case s"Sue $nr: $n1: $c1, $n2: $c2, $n3: $c3" =>
          Sue(nr.toInt, Map(n1 -> c1.toInt, n2 -> c2.toInt, n3 -> c3.toInt))

  object Found:
    val compounds: Map[String,Int] =
      Map( "children"    -> 3
         , "cats"        -> 7
         , "samoyeds"    -> 2
         , "pomeranians" -> 3
         , "akitas"      -> 0
         , "vizslas"     -> 0
         , "goldfish"    -> 5
         , "trees"       -> 3
         , "cars"        -> 2
         , "perfumes"    -> 1
         )

  val sues: List[Sue] = lines.map(Sue.fromString).toList

  lazy val answer1: Int = sues.filter(_.matches1).head.nr
  lazy val answer2: Int = sues.filter(_.matches2).head.nr
