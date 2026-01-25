package aoc2022

import nmcb.*

import scala.annotation.targetName

object Day21 extends AoC:

  type Name    = String
  type Lazy    = Long => Long
  type Result  = Long | Lazy

  object Result:

    extension (lhs: Result)

      @targetName("add")
      def |+|(rhs: Result): Result =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => lo.andThen(ro)
          case (ll: Long, ro: Lazy) => ro.compose(rl => rl - ll)
          case (lo: Lazy, rl: Long) => lo.compose(ll => ll - rl)
          case (ll: Long, rl: Long) => ll + rl

      @targetName("mul")
      def |*|(rhs: Result): Result =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => lo.andThen(ro)
          case (ll: Long, ro: Lazy) => ro.compose(rl => rl / ll)
          case (lo: Lazy, rl: Long) => lo.compose(ll => ll / rl)
          case (ll: Long, rl: Long) => ll * rl

      @targetName("sub")
      def |-|(rhs: Result): Result =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => lo.andThen(ro)
          case (ll: Long, ro: Lazy) => ro.compose(rl => ll - rl)
          case (lo: Lazy, rl: Long) => lo.compose(ll => rl + ll)
          case (ll: Long, rl: Long) => ll - rl

      @targetName("div")
      def |/|(rhs: Result): Result =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => ro.andThen(lo)
          case (ll: Long, ro: Lazy) => ro.compose(rl => ll / rl)
          case (lo: Lazy, rl: Long) => lo.compose(ll => rl * ll)
          case (ll: Long, rl: Long) => ll / rl

      @targetName("eq")
      def |=|(rhs: Result): Result =
        (lhs, rhs) match
          case (ll: Long, ro: Lazy)             => ro(ll)
          case (lo: Lazy, rl: Long)             => lo(rl)
          case (ll: Long, rl: Long) if rl == ll => rl
          case _ => sys.error("boom!")

  type Defer      = Name  => Result
  type Deferrable = Defer => Result

  sealed abstract class Expr(val name: Name, line: String) extends Deferrable:

      override def toString: String =
        s"$name=$name, line=$line\n"

  case class Val(override val name: Name, result: Long, line: String) extends Expr(name, line):

      def apply(defer: Defer): Result =
        result

  case class Bin(override val name: Name, lhs: Name, rhs: Name, op: String, line: String) extends Expr(name, line):
      import Result.*

      def apply(defer: Defer): Result =
        op match
          case "+" => defer(lhs) |+| defer(rhs)
          case "*" => defer(lhs) |*| defer(rhs)
          case "-" => defer(lhs) |-| defer(rhs)
          case "/" => defer(lhs) |/| defer(rhs)
          case "=" => defer(lhs) |=| defer(rhs)

  object Expr:

    def parseLine(line: String): Expr =
      line match
        case s"$n: $l $o $r" => Bin(n, l, r, o, s"$n: $l $o $r")
        case s"$n: $v"       => Val(n, v.toInt, s"$n: $v")

  case class SAT(input: Vector[Expr]):

    private val search: Name => Result =
      name =>
        input
          .find(_.name == name)
          .map(e => e.apply(search))
          .getOrElse(identity)

    val solve: Option[Long] =
      search("root") match
        case l: Long => Some(l)
        case _: Lazy => None


  val program: Vector[Expr] =
    lines.map(Expr.parseLine)

  val patch: Vector[Expr] =
    program.flatMap:
      case Bin("root", lhs, rhs, op, line) => Some(Bin("root", lhs, rhs, "=", line.replace(op.head, '=')))
      case m if m.name == "humn"           => None
      case m                               => Some(m)


  override lazy val answer1: Long = SAT(program).solve.getOrElse(sys.error(s"unsolved ${program.foldLeft("\n")(_ + _)}"))
  override lazy val answer2: Long = SAT(patch).solve.getOrElse(sys.error(s"unsolved ${program.foldLeft("\n")(_ + _)}"))

