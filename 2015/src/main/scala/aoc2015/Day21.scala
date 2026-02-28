package aoc2015

import aoc2015.Day21.Player.boss
import nmcb.*

object Day21 extends AoC:

  case class Player(points: Int, damage: Int, armor: Int, gold: Int):
    def enterGame: Game = Game(this, boss = boss)

  object Player:

    val boss: Player =
      Player(points = 109, damage = 8, armor = 2, gold = -1)

    def equip(points: Int, weapon: Weapon, armor: Option[Armor], rings: Seq[Ring]): Player =
      assert(rings.size <= 2, "max 2 rings")
      assert((rings diff rings.distinct).isEmpty, "max one of a kind")
      Player( points = points
            , damage = weapon.damage + rings.map(_.damage).sum
            , armor  = armor.map(_.armor).getOrElse(0) + rings.map(_.armor).sum
            , gold   = weapon.cost + armor.map(_.cost).getOrElse(0) + rings.map(_.cost).sum
            )

  enum Weapon(val cost: Int, val damage: Int):
    case Dagger     extends Weapon(8, 4)
    case ShortSword extends Weapon(10,5)
    case WarHammer  extends Weapon(25, 6)
    case Longsword  extends Weapon(40, 7)
    case GreatAxe   extends Weapon(74, 8)
    
  enum Armor(val cost: Int, val armor: Int):
    case Leather    extends Armor(13,1)
    case ChainMail  extends Armor(31, 2)
    case SplitMail  extends Armor(53, 3)
    case BandedMail extends Armor(75, 4)
    case PlateMail  extends Armor(102, 5)

  enum Ring(val cost: Int, val damage: Int, val armor: Int):
    case Damage1  extends Ring(25, 1, 0)
    case Damage2  extends Ring(50, 2, 0)
    case Damage3  extends Ring(100, 3, 0)
    case Defence1 extends Ring(20, 0, 1)
    case Defence2 extends Ring(40, 0, 2)
    case Defence3 extends Ring(80, 0, 3)

  enum Outcome derives CanEqual:
    case Won
    case Lost
    
  case class Game(player: Player, boss: Player):
    import Outcome.*

    def play: Outcome =
      val newPointsP = player.points - (if boss.damage - player.armor >= 1 then boss.damage - player.armor else 1)
      val newPointsB =   boss.points - (if player.damage - boss.armor >= 1 then player.damage - boss.armor else 1)

      if      newPointsB <= 0 then Won
      else if newPointsP <= 0 then Lost
      else
        val newP = player.copy(points = newPointsP)
        val newB = boss.copy(points = newPointsB)
        Game(newP, newB).play


  val possiblePlayers: List[Player] =
    for
      weapon  <- Weapon.values.toList
      armour  <- Armor.values.toList.map(Option(_)) :+ Option.empty[Armor]
      rings   <- Ring.values.toList.map(List(_)) ++ Ring.values.toList.combinations(2) ++ List(List.empty[Ring])
      if (rings diff rings.distinct).isEmpty
    yield
      Player.equip(100, weapon, armour, rings)


  override lazy val answer1: Int = possiblePlayers.filter(_.enterGame.play == Outcome.Won).map(_.gold).min
  override lazy val answer2: Int = possiblePlayers.filter(_.enterGame.play == Outcome.Lost).map(_.gold).max
