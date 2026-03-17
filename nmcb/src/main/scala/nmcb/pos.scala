package nmcb

object pos:

  enum Dir derives CanEqual:
    case N
    case E
    case S
    case W

    def clockWise: Dir =
      this match
        case N => E
        case S => W
        case E => S
        case W => N

    def counterClockWise: Dir =
      this match
        case N => W
        case S => E
        case E => N
        case W => S

    def opposite: Dir =
      this match
        case N => S
        case S => N
        case E => W
        case W => E

  export Dir.*

  type Pos = (x: Int, y: Int)

  object Pos:
    given CanEqual[Pos, Pos] = CanEqual.derived
    given Ordering[Pos] = Ordering.by(_.toTuple)

    val orderingByManhattanDistance: Ordering[Pos] =
      Ordering.fromLessThan((a, b) => a.manhattanDistance(Pos.origin) < b.manhattanDistance(Pos.origin))

    val orderingByPythagoreanDistance: Ordering[Pos] =
      Ordering.fromLessThan((a, b) => a.pythagoreanDistance(Pos.origin) < b.pythagoreanDistance(Pos.origin))

    val origin: Pos =
      of(0, 0)

    val offset4: Set[Pos] =
      Set(
        of( 0, -1),
        of( 0,  1),
        of(-1,  0),
        of( 1,  0)
      )

    val offset8: Set[Pos] =
      Set(
        of(-1, -1),
        of(-1,  0),
        of(-1,  1),
        of( 0, -1),
        of( 0,  1),
        of( 1, -1),
        of( 1,  0),
        of( 1,  1),
      )

    def of(x: Int, y: Int): Pos =
      (x = x, y = y)

    extension [A](it: Array[A])
      def toPos: Pos =
        assert(it.length == 2)
        (x = it(0).toString.toInt, y = it(1).toString.toInt)

  export Pos.given

  import Pos.*
  import math.*

  extension (p: Pos)

    def unary_- : Pos = (x = -p.x, y = -p.y)

    infix inline def +(that: Pos): Pos = (x = p.x + that.x, y = p.y + that.y)
    infix inline def -(that: Pos): Pos = (x = p.x - that.x, y = p.y - that.y)
    infix inline def *(i: Int): Pos    = (x = p.x * i , y = p.y * i  )

    infix def determinant(that: Pos): Long = p.x.toLong * that.y.toLong - that.x.toLong * p.y.toLong

    infix inline def translate(dx: Int = 0, dy: Int = 0): Pos = (x = p.x + dx, y = p.y + dy)

    infix inline def minimize(that: Pos): Pos = (x = p.x min that.x, y = p.y min that.y)
    infix inline def maximize(that: Pos): Pos = (x = p.x max that.x, y = p.y max that.y)

    infix inline def >(that: Pos): Boolean  = p.x > that.x && p.y > that.y
    infix inline def <(that: Pos): Boolean  = p.x < that.x && p.y < that.y
    infix inline def >=(that: Pos): Boolean = p.x >= that.x && p.y >= that.y
    infix inline def <=(that: Pos): Boolean = p.x <= that.x && p.y <= that.y

    infix inline def manhattanDistance(that: Pos): Long =
      (p.x - that.x).abs + (p.y - that.y).abs

    infix inline def pythagoreanDistance(that: Pos): Double =
      sqrt(pow((that.x - p.x).toDouble, 2) + pow((that.y - p.y).toDouble, 2))

    infix inline def angleDegrees(that: Pos): Double =
      val dx = (that.x - p.x).toDouble
      val dy = (that.y - p.y).toDouble
      val d = 90 - atan2(-dy, dx) * 180 / Pi
      if d >= 0 then d else d + 360

    def adjoint4: Set[Pos] = offset4.map(_ + p)
    def adjoint8: Set[Pos] = offset8.map(_ + p)

    infix inline def step(dir: Dir): Pos =
      dir match
        case N => (x = p.x, y = p.y - 1)
        case E => (x = p.x + 1, y = p.y)
        case S => (x = p.x, y = p.y + 1)
        case W => (x = p.x - 1, y = p.y)

    inline def stepBounded[A](dir: Dir, grid: Grid[A]): Pos =
      val result = step(dir)
      if grid.within(result) then result else p

    def directionTo(that: Pos): Vector[Dir] =
      val ew = if p.x != that.x then Vector(if that.x < p.x then W else E) else Vector.empty
      val ns = if p.y != that.y then Vector(if that.y < p.y then N else S) else Vector.empty
      ew ++ ns

    def adjointWithinBounds(min: Pos, max: Pos): Set[Pos] =
      adjoint4.filter(_.withinBounds(min, max))

    def adjWithinGrid[A](g: Grid[A], filter: ((Pos,A)) => Boolean): Set[Pos] =
      adjointWithinBounds(g.minPos, g.maxPos).filter(p => filter(p, g.peek(p)))

    def withinBounds(min: Pos, max: Pos): Boolean =
      p.x >= min.x & p.x <= max.x & p.y >= min.y & p.y <= max.y

    def withinBounds[A](grid: Grid[A]): Boolean =
      grid.within(p)

  extension [A](t: (Pos, A))
    def pos: Pos = t._1
    def element: A = t._2
