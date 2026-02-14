package aoc2015

import nmcb.*

import scala.annotation.*

object Day07 extends AoC:

  type Wire = String
  type Env  = Map[Wire, Int]

  sealed trait Rule:
    def ret: Wire
    def call(env: Env): Option[Int]

  case class Op(op: Int => Int, ret: Wire, arg: Wire) extends Rule:
    def call(env: Env): Option[Int] =
      env.get(arg).map(op)

  case class BinOp(op: Int => Int => Int, ret: Wire, lhs: Wire, rhs: Wire) extends Rule:
    def call(env: Env): Option[Int] =
      env.get(lhs).flatMap(lv => env.get(rhs).map(rv => op(lv)(rv)))

  case class Val(value: Int, ret: Wire, args: Vector[Wire] = Vector.empty) extends Rule:
    def call(env: Env): Option[Int] =
      Some(value)

  object Solver:

    def solve(rules: Vector[Rule], wire: Wire, setWireB: Option[Int] = None): Int =
      @tailrec
      def fold(rules: Seq[Rule], env: Env = Map.empty): Int =
        env.get(wire).runtimeChecked match
          case Some(v) => v
          case None    => rules match
            case rule +: rest => rule.call(env) match
              case Some(v)    => fold(rest, env.updated(rule.ret, v))
              case None       => fold(rest :+ rule, env)

      val puzzleInput: Vector[Rule] = setWireB.map(v => Val(v, "b") +: rules.filterNot(_.ret == "b")).getOrElse(rules)
      fold(puzzleInput)

  val rules: Vector[Rule] =
    def parser(s: Wire): Rule =
      s match
        case s"$lhs AND $rhs -> $ret" if lhs.toIntOption.isDefined
          => Op(rv => lhs.toInt & rv, ret, rhs)
        case s"$lhs AND $rhs -> $ret"
          => BinOp(lv => rv => lv & rv, ret, lhs, rhs)
        case s"$lhs OR $rhs -> $ret"
          => BinOp(lv => rv => lv | rv, ret, lhs, rhs)
        case s"$lhs RSHIFT $rhs -> $ret" if rhs.toIntOption.isDefined
          => Op(lv => lv >> rhs.toInt, ret, lhs)
        case s"$lhs RSHIFT $rhs -> $ret"
          => BinOp(lv => rv => lv >> rv, ret, lhs, rhs)
        case s"$lhs LSHIFT $rhs -> $ret" if rhs.toIntOption.isDefined
          => Op(lv => lv << rhs.toInt, ret, lhs)
        case s"$lhs LSHIFT $rhs -> $ret"
          => BinOp(lv => rv => lv << rv, ret, lhs, rhs)
        case s"NOT $rhs -> $ret"
          => Op(rv => ~rv & 0x0000FFFF, ret, rhs)
        case s"$arg -> $ret" if arg.toIntOption.isDefined
          => Val(arg.toInt, ret)
        case s"$rhs -> $ret"
          => Op(identity, ret, rhs)

    lines.map(parser)


  override lazy val answer1: Int = Solver.solve(rules = rules, wire = "a")
  override lazy val answer2: Int = Solver.solve(rules = rules, wire = "a", setWireB = Some(answer1))
