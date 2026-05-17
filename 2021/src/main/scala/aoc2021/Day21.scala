package aoc2021

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day21 extends AoC:

  object Part1:

    case class Dice(min: Int, max: Int, face: Int = 0, rolled: Int = 0):

      def roll: Dice =
        if face >= max then
          copy(face = min, rolled = rolled + 1)
        else
          copy(face = face + 1, rolled = rolled + 1)

    case class Player(name: String, start: Int, rolled: Seq[Int] = Seq.empty, score: Int = 0)

    case class Game(player1: Player, player2: Player, pawn1: Int, pawn2: Int, dice: Dice, goal: Int):

      @tailrec
      final def solve: Int =

        def won(player: Player): Boolean =
          player.score >= goal

        if won(player2) then
          dice.rolled * player1.score
        else
          val throw1 = dice.roll
          val throw2 = throw1.roll
          val throw3 = throw2.roll

          val position = 1 + ((pawn1 - 1 + throw1.face + throw2.face + throw3.face) % 10)
          val played   = player1.copy(score = player1.score + position)
          copy(
            player1 = player2,
            player2 = played,
            dice    = throw3,
            pawn1   = pawn2,
            pawn2   = position
          ).solve

  object Part2:

    val rollDiracDice: Vector[(Long, Long)] =
      val throws =
        for
          t1 <- 1L to 3L
          t2 <- 1L to 3L
          t3 <- 1L to 3L
        yield t1 + t2 + t3
      throws.groupMapReduce(identity)(_ => 1L)(_ + _).toVector

    case class Player(pos: Long, score: Long = 0):

      def move(steps: Long): Player =
        val to = (pos - 1 + steps) % 10 + 1
        copy(to, score + to)

    case class Game(player1: Player, player2: Player, turn: Long = 0):

      def isPlayer1Turn: Boolean =
        turn % 2 == 0

      def next(rolled: Long): Game =
        if isPlayer1Turn then
          copy(player1.move(rolled), player2, turn + 1)
        else
          copy(player1, player2.move(rolled), turn + 1)

      def hasScored(n: Int): Boolean =
        player1.score >= n || player2.score >= n

    @tailrec
    def countDiracWins(games: Vector[(Game, Long)], scores: (Long, Long) = (0L, 0L)): (Long, Long) =
      if games.isEmpty then
        scores
      else
        val (game, frequency) = games.head
        if game.hasScored(21) then
          if game.isPlayer1Turn then
            countDiracWins(games.tail, (scores.left, scores.right + frequency))
          else
            countDiracWins(games.tail, (scores.left + frequency, scores.right))
        else
          val copies = rollDiracDice.map((roll, freq) => game.next(roll) -> (frequency * freq))
          countDiracWins(copies :++ games.tail, scores)

  val player11 = Part1.Player(name = "#1", start = 7)
  val player12 = Part1.Player(name = "#2", start = 9)
  val game1 = Part1.Game(player11, player12, player11.start, player12.start, Part1.Dice(1, 100), 1000)

  val player21 = Part2.Player(pos = 7)
  val player22 = Part2.Player(pos = 9)
  val game2 = Part2.Game(player21, player22)
  lazy val (score21, score22) = Part2.countDiracWins(Vector(game2 -> 1L))

  override lazy val answer1: Long = game1.solve
  override lazy val answer2: Long = score21 max score22
