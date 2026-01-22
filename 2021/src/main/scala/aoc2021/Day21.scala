package aoc2021

import nmcb.*
import nmcb.predef.*

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

    case class Player(name: String, start: Int, rolled: Seq[Int] = Seq.empty, score: Int = 0)

    case class Game(player1: Player, player2: Player, pawn1: Int, pawn2: Int, dice: Dice, goal: Int)

    @tailrec
    def solve(g: Game): Int =

      def won(p: Player): Boolean =
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

    val player1 = Player(name = "#1", start = 7)
    val player2 = Player(name = "#2", start = 9)
    val game1 = Game(player1, player2, player1.start, player2.start, Dice(1, 100), 1000)

    lazy val answer: Int = solve(game1)


  object Part2:

    val rollDiracDice: Vector[(Long, Long)] =
      val throws =
        for
          t1 <- 1L to 3L
          t2 <- 1L to 3L
          t3 <- 1L to 3L
        yield t1 + t2 + t3
      throws.groupMapReduce(identity)(_ => 1L)(_ + _).toVector

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
    def countDiracWins(games: Vector[(Game, Long)], scores: (Long, Long)): (Long, Long) =
      games.runtimeChecked match
        case Vector() =>
          scores
        case (game, freq) +: rest if game.hasScored(21) =>
          if game.isPlayer1Turn then
            countDiracWins(rest, (scores.left, scores.right + freq))
          else
            countDiracWins(rest, (scores.left + freq, scores.right))
        case (game, current) +: rest =>
          val copies =  rollDiracDice.map((roll, freq) => game.next(roll) -> (current * freq))
          countDiracWins(copies :++ rest, scores)

    val player1 = Player(pos = 7, score = 0)
    val player2 = Player(pos = 9, score = 0)
    val game = Game(player1, player2, turn = 0)
    lazy val (score1, score2) = countDiracWins(Vector(game -> 1L), (0L, 0L))
    lazy val answer: Long = score1 max score2


  lazy val answer1: Long = Part1.answer
  lazy val answer2: Long = Part2.answer
