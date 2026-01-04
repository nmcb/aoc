package aoc2019

import nmcb.*
import scala.annotation.tailrec

object Day13 extends AoC:

  import cpu.*

  enum Tile(val id: Long):
    case Empty  extends Tile(0)
    case Wall   extends Tile(1)
    case Block  extends Tile(2)
    case Paddle extends Tile(3)
    case Ball   extends Tile(4)

  object Tile:
    def fromId(id: Long): Tile =
      Tile.values.find(_.id == id).get

  import Tile.*

  case class Location(x: Long, y: Long)

  extension (outputs: LazyList[Value])
    def render: Map[Location,Tile] =
      outputs.grouped(3).foldLeft(Map.empty[Location,Tile]):
        case (result, LazyList(x,y,id)) => result + (Location(x,y) -> Tile.fromId(id))
        case output                     => sys.error(s"invalid output: $output")

  val program: Mem = Mem.parse(input)

  type Pixel = (Location, Tile)

  extension (pixel: Pixel)
    def location: Location   = pixel._1
    def tile: Tile           = pixel._2

  case class GameState(paddle: Option[Location], ball: Option[Location], score: Option[Value], display: Map[Location,Tile]):

    def updated(loc: Location, value: Value): GameState =
      (loc,value) match
        case (Location(-1, 0), score) => copy(score   = Some(score))
        case (loc, 3)                 => copy(paddle  = Some(loc), display = display.updated(loc, Paddle))
        case (loc, 4)                 => copy(ball    = Some(loc), display = display.updated(loc, Ball))
        case (loc, tile)              => copy(display = display.updated(loc, Tile.fromId(tile)))

    def echo(): Unit =
      val Home   = "\u001b[H"
      val Clear  = "\u001b[2J"
      val Bold   = "\u001b[1m"
      val Reset  = "\u001b[0m"
      val Green  = "\u001b[32m"
      val Yellow = "\u001b[33m"
      val Red    = "\u001b[31m"
      val Blue   = "\u001b[34m"
      val White  = "\u001b[97m"

      val sprites =
        Map(
          Empty  -> (               " "         ),
          Wall   -> (Green +        "█"         ),
          Block  -> (Blue  +        "░"         ),
          Paddle -> (White + Bold + "‒" + Reset),
          Ball   -> (Red   + Bold + "O"  + Reset),
        )

      val buffer = collection.mutable.ListBuffer(Home + Clear)
      buffer += s"$White$Bold  Score: ${score.getOrElse(666)}  Blocks: ${display.values.count(_ == Block)} $Reset\n"

      val maxX = display.maxBy(_.location.x).location.x.toInt
      val maxY = display.maxBy(_.location.y).location.y.toInt

      for y <- 0 to maxY do
        for x <- 0 to maxX do
          buffer += sprites(display(Location(x,y)))
        buffer += "\n"
      buffer += Reset

      println(buffer.mkString)
      Thread.sleep(20)


  def play(program: Mem, onScreen: Boolean): Value =
    @tailrec
    def go(cpu: CPU, game: GameState): Value =
      val executions = cpu.executeAll
      if executions.isEmpty then
        game.score.get
      else
        val outputs = executions.flatMap(_.outputOption)
        val frame   = outputs.grouped(3).foldLeft(game):
          case (result, LazyList(x,y,value)) => result.updated(Location(x,y), value)
          case (_, _)                        => sys.error("incomplete output")

        val ballX   = frame.ball.get.x
        val paddleX = frame.paddle.get.x
        val input   = ballX.compareTo(paddleX).sign
        val next    = executions.last.cpu.copy(stdin = LazyList(input))

        if onScreen then frame.echo()
        go(next, frame)

    go(CPU(program + (0 -> 2L)), GameState(None, None, None, Map.empty.withDefaultValue(Empty)))

  lazy val answer1: Int   = CPU(program).outputs.render.values.count(_ == Block)
  lazy val answer2: Value = play(program, onScreen = false)
