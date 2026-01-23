package examples

object Year201Day21Part2_Scala374vsScala381:

  case class Pawn(pos: Int, score: Int = 0):

    def move(steps: Int): Pawn =
      val nextPos = ((pos - 1 + steps) % 10) + 1
      Pawn(nextPos, score + nextPos)


  val rollDiracDice: Map[Int, Int] =
    val throws =
      for
        t1 <- 1 to 3
        t2 <- 1 to 3
      yield t1 + t2

    throws.groupMapReduce(identity)(_ => 1)(_ + _)


  def play(pawn1: Pawn, pawn2: Pawn): Long =

    def go(pawn1: Pawn, pawn2: Pawn): (Long, Long) =
      println(s"called: pawn1=$pawn1, pawn2=$pawn2")

      def winOrTurn(pawn1: Pawn, pawn2: Pawn): (Long, Long) =
        if pawn1.score >= 2 then (1, 0) else go(pawn2, pawn1).swap

      val turns: Iterable[(Long, Long)] =
        for
          (roll, count) <- rollDiracDice
          moved          = pawn1.move(roll)
          (u1, u2)       = winOrTurn(moved, pawn2)
        yield
          println(s"yielding")
          (count * u1, count * u2)

      turns.reduce:
        case ((u1a, u2a), (u1b, u2b)) =>
          println(s"reducing")
          (u1a + u1b, u2a + u2b)

    val (score1, score2) = go(pawn1, pawn2)
    score1 max score2

  @main
  def printResult(): Unit =
    println(s"result: ${play(Pawn(pos = 7), Pawn(pos = 9))}")


  def f(x: Int): (Int, Int) = (1, 0)

  val result: Iterable[(Int, Int)] =
    for
      (k, v) <- Map(1 -> 1, 2 -> 1, 3 -> 1)
      x       = k + v
      (a, b)  = f(x)
    yield (a, b)

  /** @see https://github.com/scala/scala3/issues/25077 */
  @main def minimizedCode(): Unit =
    assert(result.size == 3, s"Expected 3 elements, got ${result.size}")
