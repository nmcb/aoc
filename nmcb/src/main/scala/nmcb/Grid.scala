package nmcb

import Dijkstra.*
import pos.*

case class Grid[+A](matrix: Vector[Vector[A]]):
  val sizeY: Int = matrix.size
  val sizeX: Int = matrix.head.size
  val minPos: Pos = Pos.origin
  val maxPos: Pos = (sizeX - 1, sizeY - 1)
  val leftUpper: Pos   = minPos
  val leftBottom: Pos  = (minPos.x, maxPos.y)
  val rightUpper: Pos  = (maxPos.x, minPos.y)
  val rightBottom: Pos = maxPos

  lazy val positions: Set[Pos] =
    (for {x <- 0 until sizeX; y <- 0 until sizeY} yield (x, y)).toSet

  inline def elements[B >: A]: Set[(Pos, B)] =
    positions.map(p => p -> peek(p))

  inline def within(p: Pos): Boolean =
    p.withinBounds(minPos, maxPos)

  inline def peek(p: Pos): A =
    peek(p.x, p.y)

  inline def peek(x: Int, y: Int): A =
    matrix(y)(x)

  inline def contains[B >: A](p: Pos, a: B): Boolean =
    peekOption(p).contains(a)

  inline def peekOption(p: Pos): Option[A] =
    Option.when(p.withinBounds(minPos, maxPos))(peek(p))

  inline def peekOrElse[B >: A](p: Pos, default: => B): B =
    peekOption(p).getOrElse(default)

  inline def find[B >: A](a: B)(using CanEqual[A, B]): Option[Pos] =
    elements.find(_.element == a).map(_.pos)

  inline def findAll[B >: A](a: B)(using CanEqual[A, B]): Set[Pos] =
    elements.filter(_.element == a).map(_.pos)

  inline def findOne[B >: A](a: B, default: => Pos = sys.error(s"not found"))(using CanEqual[A, B]): Pos =
    find(a).getOrElse(default)

  inline def filter[B >: A](f: ((Pos, B)) => Boolean): Set[(Pos, B)] =
    elements.filter(f)

  inline def filterNot[B >: A](f: ((Pos, B)) => Boolean): Set[(Pos, B)] =
    elements.filterNot(f)

  inline def map[B](f: A => B): Grid[B] =
    Grid(matrix.map(_.map(f)))

  inline def mapElement[B](f: (Pos, A) => B): Grid[B] =
    Grid(
      matrix
        .zipWithIndex.map: (row, y) =>
          row.zipWithIndex.map: (a, x) =>
            f((x, y), a)
    )

  inline def row(y: Int): Vector[A] =
    matrix(y)

  inline def updated[B >: A](p: Pos, a: B): Grid[B] =
    Grid(matrix.updated(p.y, row(p.y).updated(p.x, a)))

  inline def asString: String =
    matrix.map(_.mkString("")).mkString("\n")

  inline def extractPath[B >: A](from: B, to: B, node: B)(using CanEqual[A, B]): (Pos, Pos, Grid[B]) =
    val fromPos  = findOne(from)
    val toPos    = findOne(to)
    val cleared  = updated(fromPos, node).updated(toPos, node)
    (fromPos, toPos, cleared)

  inline def dropRow(y: Int): Grid[A] =
    Grid(matrix.zipWithIndex.filter((r, i) => i != y).map((r, i) => r))

  inline def transpose: Grid[A] =
    Grid(matrix.transpose)

  inline def flipX: Grid[A] =
    Grid(matrix.map(_.reverse))

  inline def flipY: Grid[A] =
    Grid(matrix.reverse)

  inline def rotateCW: Grid[A] =
    def rotateMatrixCW(matrix: Vector[Vector[A]]): Vector[Vector[A]] =
      if matrix.isEmpty then
        matrix
      else
        Vector.tabulate(sizeX, sizeY)((x, y) => matrix(sizeX - 1 - y)(x))
    Grid(rotateMatrixCW(matrix))

  inline def toMap[B >: A]: Map[Pos, Set[(Pos, B)]] =
    positions.map(p => p -> p.adjoint4.filter(within).map(n => n -> peek(n))).toMap

object Grid:
  
  def empty[A]: Grid[A] =
    Grid(Vector(Vector.empty[A]))

  def fromLines(lines: Iterator[String]): Grid[Char] =
    Grid(lines.map(_.toVector).toVector)

  def fromLines(lines: Seq[String]): Grid[Char] =
    Grid(lines.map(_.toVector).toVector)

  def fromString[A](s: String): Grid[Char] =
    fromLines(s.trim.split('\n').iterator)

  def fromMatrix[A](matrix: Iterator[Seq[A]]): Grid[A] =
    Grid(matrix.iterator.to(Vector).map(_.iterator.to(Vector)))

  def fromMatrix[A](matrix: Vector[Vector[A]]): Grid[A] =
    Grid(matrix)

  def fill[A](sizeX: Int, sizeY: Int, default: A): Grid[A] =
    Grid(Vector.fill(sizeX, sizeY)(default))

  extension [A](g: (Pos, Pos, Grid[A]))
    def from: Pos = g._1
    def to: Pos   = g._2
    def cleared: Grid[A] = g._3

    def shortest: Vector[Pos] =
      import Pos.given
      given CanEqual[A, A] = CanEqual.derived
      Dijkstra
        .run(from, Graph.fromGrid(cleared, cleared.peek(from)))
        .pathTo(to)
        .toTrail
