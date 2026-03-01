package aoc2017

import nmcb.*
import nmcb.pos.*
import nmcb.predef.*

object Day22 extends AoC:

  enum Status derives CanEqual:
    case Clean
    case Infected
    case Weakened
    case Flagged

  import Status.*
  
  case class Carrier(nodes: Map[Pos,Status], current: Pos, dir: Dir, infected: Int = 0):

    def wake1: Carrier =
      nodes(current).runtimeChecked match
        case Clean =>
          val turn = dir.ccw
          copy(
            nodes    = nodes + (current -> Infected),
            current  = current.step(turn),
            dir      = turn,
            infected = infected + 1
          )
        case Infected =>
          val turn = dir.cw
          copy(
            nodes   = nodes + (current -> Clean),
            current = current.step(turn),
            dir     = turn
          )

    def wake2: Carrier =
      nodes(current).runtimeChecked match
        case Clean =>
          val turn = dir.ccw
          copy(
            nodes   = nodes + (current -> Weakened),
            current = current.step(turn),
            dir     = turn
          )
        case Weakened =>
          copy(
            nodes    = nodes + (current -> Infected),
            current = current.step(dir),
            infected = infected + 1
          )
        case Infected =>
          val turn = dir.cw
          copy(
            nodes   = nodes + (current -> Flagged),
            current = current.step(turn),
            dir     = turn
          )
        case Flagged =>
          val turn = dir.opposite
          copy(
            nodes   = nodes + (current -> Clean),
            current = current.step(turn),
            dir     = turn
          )

  val carrier: Carrier =

    val current: Pos =
      (lines(0).length / 2, lines.length / 2)

    val nodes: Map[Pos,Status] =
      lines
        .zipWithIndex
        .flatMap: (row,y) =>
          row
            .zipWithIndex
            .collect:
              case ('#',x) => (x, y) -> Infected
              case ('.',x) => (x, y) -> Clean
        .toMap
        .withDefaultValue(Clean)

    Carrier(nodes, current, Dir.N)

  def solve(init: Carrier, iterations: Int)(next: Carrier => Carrier): Int =
    Iterator.iterate(init)(next).nth(iterations).infected

  override lazy val answer1: Int = solve(carrier, 10000)(_.wake1)
  override lazy val answer2: Int = solve(carrier, 10000000)(_.wake2)
