package aoc2018

import nmcb.*
import nmcb.pos.*

object Day03 extends AoC:

  type Claims   = Vector[Vector[Pos]]
  type Overlaps = Map[Pos,Int]

  val (claims: Claims, overlaps: Overlaps) =
    val claims = lines.map:
      case s"#${id} @ ${x},${y}: ${w}x${h}" =>
        val x0 = x.toInt
        val x1 = w.toInt + x0
        val y0 = y.toInt
        val y1 = h.toInt + y0
        (for x <- x0 until x1 ; y <- y0 until y1 yield Pos.of(x,y)).toVector

    val overlaps = claims.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    (claims, overlaps)

  override lazy val answer1: Int = overlaps.valuesIterator.count(_ >= 2)
  override lazy val answer2: Int = 1 + claims.indexWhere(claim => claim.forall(pos => overlaps(pos) == 1))

