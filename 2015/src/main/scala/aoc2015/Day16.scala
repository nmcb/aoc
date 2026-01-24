package aoc2015

import nmcb.*

object Day16 extends AoC:

  case class Sue(nr: Int, properties: Map[String,Int]):
    def matches1(that: Map[String, Int]): Boolean =
      properties.forall((name, count) =>
        that.get(name).contains(count))

    def matches2(that: Map[String, Int]): Boolean =
      properties.forall((name, count) =>
        name match
          case "cats" | "trees"           => that.get(name).exists(_ < count)
          case "pomeranians" | "goldfish" => that.get(name).exists(_ > count)
          case _                          => that.get(name).contains(count))

  object Sue:
    def fromString(s: String): Sue =
      s match
        case s"Sue $nr: $n1: $c1, $n2: $c2, $n3: $c3" =>
          Sue(nr.toInt, Map(n1 -> c1.toInt, n2 -> c2.toInt, n3 -> c3.toInt))

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

  override lazy val answer1: Int = sues.find(_.matches1(compounds)).getOrElse(sys.error("not found")).nr
  override lazy val answer2: Int = sues.find(_.matches2(compounds)).getOrElse(sys.error("not found")).nr
