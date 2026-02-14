package aoc2023

import nmcb.*
import nmcb.pos.*

import scala.annotation.tailrec

object Day18 extends AoC:

  case class Op(direction: Char, length: Int)

  object Op:
    def fromStringPart(s: String): Op =
      s match
        case s"$direction $length (#$hex)" =>
          Op(direction.head, length.toInt)

    def fromHexPart(s: String): Op =
      s match
        case s"$direction $length (#$hex)" =>
          val direction =
            hex.last match
              case '0' => 'R'
              case '1' => 'D'
              case '2' => 'L'
              case '3' => 'U'
          Op(direction, Integer.parseInt(hex.init, 16))

  extension (p: Pos)

    infix def move1(op: Op): Vector[Pos] =
      op.runtimeChecked match
        case Op('U', length) => Vector.tabulate(length)(dy => (x = p.x, y = p.y - dy - 1))
        case Op('D', length) => Vector.tabulate(length)(dy => (x = p.x, y = p.y + dy + 1))
        case Op('L', length) => Vector.tabulate(length)(dx => (x = p.x - dx - 1, y = p.y))
        case Op('R', length) => Vector.tabulate(length)(dx => (x = p.x + dx + 1, y = p.y))

    infix def move2(op: Op): Pos =
      op.runtimeChecked match
        case Op('U', length) => (x = p.x, y = p.y - length)
        case Op('D', length) => (x = p.x, y = p.y + length)
        case Op('L', length) => (x = p.x - length, y = p.y)
        case Op('R', length) => (x = p.x + length, y = p.y)



  def grid(min: Pos, max: Pos): IndexedSeq[Pos] =
    for
      x <- min.x to max.x
      y <- min.y to max.y
    yield
      Pos.of(x, y)

  /** cubic meter = one point - data driven flood algorithm with some set arithmetic */
  def dig1(operations: Vector[Op]): Long =
    val holes: Set[Pos] = operations.foldLeft(Vector(Pos.origin))((a,o) => a ++ a.last.move1(o)).toSet
    val min = holes.reduce(_ min _) - Pos.of(1, 1)
    val max = holes.reduce(_ max _) + Pos.of(1, 1)

    @tailrec
    def flood(todo: List[Pos], visited: Set[Pos] = Set.empty): Set[Pos] =
      todo match
        case Nil =>
          visited
        case cur :: rest =>
          val reached =
            cur
              .adjoint4
              .diff(holes)
              .diff(visited)
              .filter(box => box >= min && box <= max)

          flood(rest ++ reached, visited ++ reached + cur)

    val outer = flood(List(min))
    val dug   = grid(min, max).toSet diff outer
    dug.size.toLong

  lazy val operationsPart1: Vector[Op] = lines.map(Op.fromStringPart)

  /** one operation = one polygon - geometric driven shoelace formula made discrete with pick's theorem */
  def dig2(operations: Vector[Op]): Long =

    // shoelace formula
    def shoeLace(vertices: Vector[Pos]): Long =
      val result =
        (vertices :+ vertices.head)
          .sliding(2)
          .map(m => m(0) ⋅ m(1))
          .sum / 2
      result.abs

    val vertices = operations.scanLeft(Pos.origin)(_ move2 _)
    val geometricArea = shoeLace(vertices)

    // pick's theorem
    val edge = operations.map(_.length.toLong).sum
    val interior = geometricArea - edge / 2 + 1
    val discreteArea = edge + interior

    discreteArea

  assert(answer1 == dig2(operationsPart1)) // 1000x faster

  lazy val operationsPart2: Vector[Op] = lines.map(Op.fromHexPart)


  override lazy val answer1: Long = dig1(operationsPart1)
  override lazy val answer2: Long = dig2(operationsPart2)
