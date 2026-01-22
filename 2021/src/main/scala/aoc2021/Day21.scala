package aoc2021

import nmcb.*

import scala.annotation.tailrec

object Day21 extends AoC:

  object Part1:

    case class Dice(min: Int, max: Int, cur: Int = 0, rolled: Int = 0):

      def face: Int =
        cur

      def roll: Dice =
        if cur >= max then
          copy(cur = min, rolled = rolled + 1)
        else
          copy(cur = cur + 1, rolled = rolled + 1)

    case class PlayerPart1(name: String, start: Int, rolled: Seq[Int] = Seq.empty, score: Int = 0)

    case class GamePart1(player1: PlayerPart1, player2: PlayerPart1, pawn1: Int, pawn2: Int, dice: Dice, goal: Int)

    @tailrec
    def solve(g: GamePart1): Int =

      def won(p: PlayerPart1): Boolean =
        p.score >= g.goal

      if won(g.player2) then
        g.dice.rolled * g.player1.score
      else
        val throw1 = g.dice.roll
        val throw2 = throw1.roll
        val throw3 = throw2.roll

        val position = 1 + ((g.pawn1 - 1 + throw1.face + throw2.face + throw3.face) % 10)
        val played   = g.player1.copy(score = g.player1.score + position)
        solve(
          g.copy(
            player1 = g.player2,
            player2 = played,
            dice = throw3,
            pawn1 = g.pawn2,
            pawn2 = position
          )
        )

    val player1 = PlayerPart1(name = "#1", start = 7)
    val player2 = PlayerPart1(name = "#2", start = 9)
    val game1 = GamePart1(player1, player2, player1.start, player2.start, Dice(1, 100), 1000)

    lazy val answer1: Int = solve(game1)


  object Part2:

    val rollDiracDice: Map[Long, Long] =
      val throws =
        for
          t1 <- 1L to 3L
          t2 <- 1L to 3L
          t3 <- 1L to 3L
        yield t1 + t2 + t3
      throws.groupMapReduce(identity)(_ => 1L)(_ + _)

    case class Player(pos: Long, score: Long):

      def move(n: Long): Player =
        val newPos = (pos - 1 + n) % 10 + 1
        copy(newPos, score + newPos)

    case class Game(player1: Player, player2: Player, turn: Long):

      def isPlayer1Turn: Boolean =
        turn % 2 == 0

      def next(rolled: Long): Game =
        if isPlayer1Turn then
          copy(player1.move(rolled), player2, turn + 1)
        else
          copy(player1, player2.move(rolled), turn + 1)

      def hasScored(n: Int): Boolean =
        player1.score >= n || player2.score >= n

    @annotation.tailrec
    def countDiracWins(games: List[(Game, Long)], tally: (Long, Long)): (Long, Long) =
      games match
        case Nil => tally
        case (game, freq) :: tail if game.hasScored(21) =>
          if game.isPlayer1Turn then countDiracWins(tail, (tally._1, tally._2 + freq))
          else countDiracWins(tail, (tally._1 + freq, tally._2))
        case (game, freq) :: tail =>
          val copies =
            for (roll, dieFreq) <- rollDiracDice.toList
              yield game.next(roll) -> (dieFreq * freq)
          countDiracWins(copies ::: tail, tally)

    val player1 = Player(pos = 7, score = 0)
    val player2 = Player(pos = 9, score = 0)
    val game = Game(player1, player2, turn = 0)
    lazy val (score1, score2) = countDiracWins(List(game -> 1L), (0L, 0L))
    lazy val answer: Long = score1 max score2


  lazy val answer1: Long = Part1.answer1
  lazy val answer2: Long = Part2.answer
