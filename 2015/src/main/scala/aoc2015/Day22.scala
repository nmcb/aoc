package aoc2015

import nmcb.*

object Day22 extends AoC:

  enum Spell(val cost: Int, val duration: Int) derives CanEqual:
    case MagicMissile extends Spell(53, 1)
    case Drain        extends Spell(73, 1)
    case Shield       extends Spell(113, 6)
    case Poison       extends Spell(173, 6)
    case Recharge     extends Spell(229, 5)

  import Spell.*
  
  val Spells: Set[Spell] = Set(MagicMissile, Drain, Shield, Poison, Recharge)

  case class Game(
     player: Int,
     mana:   Int,
     spent:  Int,
     armor:  Int,
     boss:   Int,
     damage: Int,
     spells: Map[Spell,Int] = Map.empty
  ):

    extension (active: (Spell, Int))
      def spell: Spell  = active._1
      def duration: Int = active._2

    def nextGame: Game =
      spells.foldLeft(copy(armor = 0))(_ runsSpells _)

    infix def runsSpells(active: (Spell, Int)): Game =
      val next = effect(active.spell)
      if active.duration == 1 then
        next.copy(spells = spells.removed(active.spell))
      else
        next.copy(spells = spells.updated(active.spell, active.duration - 1))

    def effect(spell: Spell): Game =
      spell match
        case MagicMissile => copy(boss = boss - 4)
        case Drain        => copy(player = player + 2, boss = boss - 2)
        case Shield       => copy(armor = 7)
        case Poison       => copy(boss = boss - 3)
        case Recharge     => copy(mana = mana + 101)

    def playerTurn(spell: Spell): Game =
      copy(
        mana    = mana - spell.cost,
        spent   = spent + spell.cost,
        spells  = spells + (spell -> spell.duration)
      )

    def bossTurn: Game =
      copy(player = player - (damage - armor).max(1))

    def hardMode: Game =
      copy(player = player - 1)

    def play(hard: Boolean): Int =

      var result = Int.MaxValue

      def loop(game: Game, playersTurn: Boolean): Unit =
        val before = if hard && playersTurn then game.hardMode else game
        val after = before.nextGame

        if before.spent >= result || before.player <= 0 then ()
        else if after.boss <= 0 then result = result min after.spent
        else if !playersTurn then loop(after.bossTurn, !playersTurn)
        else
          Spells
            .diff(after.spells.keySet)
            .filter(_.cost <= after.mana)
            .foreach(spell => loop(after.playerTurn(spell), !playersTurn))

      loop(this, true)

      result


  val game: Game =
    Game(
      player = 50,
      mana   = 500,
      spent  = 0,
      armor  = 0,
      boss   = 71,
      damage = 10
    )

  override lazy val answer1: Int = game.play(hard = false)
  override lazy val answer2: Int = game.play(hard = true)
