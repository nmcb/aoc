package examples

import nmcb.predef.*

import java.lang.System.*
import scala.Option.when
import scala.collection.*
import scala.util.*


object FriedmanNumber:

  trait Operation derives CanEqual:
    def run(l: Int, r: Int): Option[Int]

  case object Addition extends Operation:
    def run(l: Int, r: Int): Option[Int] =
      Some(l + r)

  case object Subtraction extends Operation:
    def run(l: Int, r: Int): Option[Int] =
      Some(l - r)

  case object Multiplication extends Operation:
    def run(l: Int, r: Int): Option[Int] =
      Some(l * r)

  case object Division extends Operation:
    def run(l: Int, r: Int): Option[Int] =
      when(r != 0 && l % r == 0)(l / r)

  case object Exponentiation extends Operation:
    def run(l: Int, r: Int): Option[Int] =
      Some(math.pow(l, r).toInt)

  case object Concatenation extends Operation:
    def run(l: Int, r: Int): Option[Int] =
      sys.error(s"unimplemented, marker operation only which has special handling in computation order")

  object Operation:
    val all: Vector[Operation] =
      Vector(
        Addition,
        Subtraction,
        Multiplication,
        Division,
        Exponentiation,
        Concatenation
      )


  sealed trait ComputationOrder derives CanEqual:

    def isConcat: Boolean =
      this match
        case BinaryOperation(l, r, Concatenation) => l.isConcat && r.isConcat
        case Value(v)                             => true
        case _                                    => false

    def compute: Option[Int] =
      this.runtimeChecked match
        case Value(v)                             => Some(v)
        case BinaryOperation(l, r, Concatenation) => when(l.isConcat && r.isConcat)(s"${l.compute.get}${r.compute.get}".toInt)
        case BinaryOperation(l, r, o)             => l.compute.flatMap(lr => r.compute.flatMap(rr => o.run(lr, rr)))

    def bind(operations: Vector[Operation], arguments: Vector[Int]): ComputationOrder =
      val (result, _, _) = bindRecursively(operations, arguments)
      result

    private def bindRecursively(operations: Vector[Operation], arguments: Vector[Int]): (ComputationOrder, Vector[Operation], Vector[Int]) =
      this match
        case Value(v) =>
          (Value(arguments.head), operations, arguments.tail)
        case BinaryOperation(l, r, v) =>
          val (fl, ol, al) = l.bindRecursively(operations.tail, arguments)
          val (fr, or, ar) = r.bindRecursively(ol, al)
          (BinaryOperation(fl, fr, operations.head), or, ar)

  case class Value(v: Int = 0)                                                              extends ComputationOrder
  case class BinaryOperation(l: ComputationOrder, r: ComputationOrder, o: Operation = null) extends ComputationOrder


  private val possibleOperationsMemo = Memo.empty[Int, Vector[Vector[Operation]]]
  def possibleOperations(size: Int): Vector[Vector[Operation]] =
    possibleOperationsMemo.memoize(size):
      Vector
        .fill(size)(Operation.all)
        .flatten
        .combinations(size).flatMap(_.permutations)
        .filter(_.count(_ == Concatenation) != size)
        .distinct
        .toVector

  private val possibleDigitSequencesMemo = Memo.empty[Vector[Int], Vector[Vector[Int]]]
  def possibleDigitSequences(digits: Vector[Int]): Vector[Vector[Int]] =
    possibleDigitSequencesMemo.memoize(digits):
      digits
        .combinations(digits.length)
        .flatMap(_.permutations)
        .distinct
        .toVector

  private val possibleComputationOrdersMemo = Memo.empty[Int, Vector[ComputationOrder]]
  def possibleComputationOrders(size: Int): Vector[ComputationOrder] =
    possibleComputationOrdersMemo.memoize(size):
      size match
        case 0 => Vector.empty
        case 1 => Vector(Value())
        case n =>
          (0 until size).toVector.flatMap: leftSize =>
            for
              t1 <- possibleComputationOrders(leftSize)
              t2 <- possibleComputationOrders(size - leftSize - 1)
            yield
              BinaryOperation(t1, t2)


  def isFriedmanNumber(number: Int): Boolean =
    val digits: Vector[Int] = number.toString.toVector.map(_.asDigit)
    possibleOperations(digits.length - 1).exists: operations =>
        possibleDigitSequences(digits).exists: sequence =>
          possibleComputationOrders(operations.size + sequence.size).exists: order =>
            order.bind(operations, sequence).compute.contains(number)

  def sieve(max: Int): Seq[Int] =
    (0 to max).filter(isFriedmanNumber)


  def main(args: Array[String]): Unit =
    val max: Int         = Try(args(0).toInt).getOrElse(10000)
    val start: Long      = currentTimeMillis
    val result: Seq[Int] = sieve(max)

    println(s"friedman numbers up to $max:")
    println(result.mkString(", "))
    println(s"took ${currentTimeMillis - start} ms")