package aoc2022

import nmcb.*
import nmcb.parsing.*

object Day13 extends AoC:

  enum E derives CanEqual:
    case N(n: Long)
    case L(l: List[E])
    
  object E:
    def L(es: E*): L = L(es.toList)

    def n: P[E] = digits.map(N.apply)
    def l: P[E] = seq('[', ',', ']', e).map(L.apply)
    def e: P[E] = n | l
    def parse(s: String): E = e.run(s)

    given ordering: Ordering[E] with
      def compare(l: E, r: E): Int =
        (l,r) match
          case (ln: N, rl: L)             => compare(L(ln), rl)
          case (ll: L, rn: N)             => compare(ll, L(rn))
          case (N(ln), N(rn))             => ln.compare(rn)
          case (L(Nil), L(_ :: _))        => -1
          case (L(Nil), L(Nil))           =>  0
          case (L(_ :: _), L(Nil))        =>  1
          case (L(lh :: lt), L(rh :: rt)) =>
            compare(lh, rh) match
               case -1 => -1
               case  0 => compare(L(lt), L(rt))
               case  1 => 1


  import E.*

  import math.Ordered.orderingToOrdered

  val expressions: List[E] =
    lines
      .filterNot(_.isBlank)
      .map(parse)
      .toList

  
  override lazy val answer1: Int =
    expressions
      .grouped(2)
      .zipWithIndex
      .map((es,idx) => if es(0) <= es(1) then idx + 1 else 0)
      .sum

  override lazy val answer2: Long =
    val divider1: E = L(L(N(2)))
    val divider2: E = L(L(N(6)))

    val ordered = (divider1 :: divider2 :: expressions).sorted
    val idx1 = ordered.indexWhere(_ == divider1) + 1
    val idx2 = ordered.indexWhere(_ == divider2) + 1
    idx1 * idx2
