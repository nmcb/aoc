package nmcb

object pos:

  enum Dir:
    case N, E, S, W

    def cw: Dir =
      this match
        case N => E
        case S => W
        case E => S
        case W => N

    def ccw: Dir =
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

  case class Pos(x: Int, y: Int):

    def unary_- : Pos = Pos(-x, -y)

    infix inline def +(p: Pos): Pos = copy(x = x + p.x, y = y + p.y)
    infix inline def -(p: Pos): Pos = copy(x = x - p.x, y = y - p.y)
    infix inline def *(i: Int): Pos = copy(x = x * i  , y = y * i  )

    def translate(dx: Int, dy: Int): Pos = Pos(x + dx, y + dy)

    def distance(o: Pos): Double =
      val dx = o.x - x
      val dy = o.y - y
      math.sqrt(math.pow(dx.toDouble, 2) + math.pow(dy.toDouble, 2))

    def angle(o: Pos): Double =
      val dx = (o.x - x).toDouble
      val dy = (o.y - y).toDouble
      val d = 90 - math.atan2(-dy, dx) * 180 / math.Pi
      if d >= 0 then d else d + 360

    def adjoint4: Set[Pos] =
      Pos.offset4.map(_ + this)

    def adjoint8: Set[Pos] =
      Pos.offset8.map(_ + this)

    infix inline def step(dir: Dir): Pos =
      dir match
        case N => copy(y = y - 1)
        case E => copy(x = x + 1)
        case S => copy(y = y + 1)
        case W => copy(x = x - 1)

    def directionTo(neighbour: Pos): Vector[Dir] =
      val ew = if x != neighbour.x then Vector(if neighbour.x < x then W else E) else Vector.empty
      val ns = if y != neighbour.y then Vector(if neighbour.y < y then N else S) else Vector.empty
      ew ++ ns

    def adjointWithinBounds(min: Pos, max: Pos): Set[Pos] =
      adjoint4.filter(_.withinBounds(min, max))

    def adjWithinGrid[A](g: Grid[A], filter: ((Pos,A)) => Boolean): Set[Pos] =
      adjointWithinBounds(g.minPos, g.maxPos).filter(p => filter(p, g.peek(p)))

    def withinBounds(min: Pos, max: Pos): Boolean =
      x >= min.x & x <= max.x & y >= min.y & y <= max.y

    infix def manhattan(p: Pos): Long =
      math.abs(x - p.x) + math.abs(y - p.y)

  object Pos:

    val origin: Pos =
      of(0, 0)

    val offset4: Set[Pos] =
      Set(
        of( 0,-1),
        of( 0, 1),
        of(-1, 0),
        of( 1, 0)
      )

    val offset8: Set[Pos] =
      Set(
        of(-1, -1),
        of(-1,  0),
        of(-1,  1),
        of( 0, -1),
        of( 0, 1),
        of( 1, -1),
        of( 1,  0),
        of( 1,  1),
      )

    inline def of(x: Int, y: Int): Pos =
      Pos(x, y)

    extension (tuple: (Int, Int))
      def toPos: Pos = Pos(tuple._1, tuple._2)

    extension [A](it: Array[A])
      def toPos: Pos =
        assert(it.length == 2)
        Pos(it(0).toString.toInt, it(1).toString.toInt)
