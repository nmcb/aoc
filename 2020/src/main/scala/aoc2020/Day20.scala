package aoc2020

import nmcb.*
import nmcb.pos.*
import nmcb.predef.*

import scala.annotation.tailrec

object Day20 extends AoC:


  val monster: Seq[Pos] =
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #
      |"""
      .stripMargin
      .split("\n")
      .toSeq
      .zipWithIndex
      .flatMap: (line,y) =>
        line
          .zipWithIndex
          .collect:
            case (c,x)if c == '#' => (x, y)

  /** note that images are squares */
  case class Image(size: Int, pixels: Map[Pos, Char]):
    val top: IndexedSeq[Char]    = (0 until size).map(x => pixels((x, 0)))
    val left: IndexedSeq[Char]   = (0 until size).map(y => pixels((0, y)))
    val bottom: IndexedSeq[Char] = (0 until size).map(x => pixels((x, size - 1)))
    val right: IndexedSeq[Char]  = (0 until size).map(y => pixels((size - 1, y)))

    private def rotateCW: Image =
      Image(size, pixels.map((pos, pixel) => (size - pos.y - 1, pos.x) -> pixel))

    private def flipH: Image =
      Image(size, pixels.map((pos, pixel) => (size - pos.x - 1, pos.y) -> pixel))

    private def flipV: Image =
      Image(size, pixels.map((pos, pixel) => (pos.x, size - pos.y - 1) -> pixel))

    def permutations: Vector[Image] =
      import Iterator.*
      val all =
        for
          rotate <- 0 to 3
          fliph  <- 0 to 1
          flipv  <- 0 to 1
        yield
          val rotated = iterate(this)(_.rotateCW).nth(rotate)
          val flipped = iterate(rotated)(_.flipH).nth(fliph)
          iterate(flipped)(_.flipV).nth(flipv)
      all.distinct.toVector

  case class Tile(id: Long, image: Image):
    export image.{top, left, bottom, right}

    val edges: Set[IndexedSeq[Char]] =
      val forward   = Set(top, left, bottom, right)
      val backwards = forward.map(_.reverse)
      forward ++ backwards

    def adjacentTo(other: Tile): Boolean =
      other.edges.intersect(edges).nonEmpty

    def permutations: Seq[Tile] =
      image.permutations.map(Tile(id, _))

  def corners(tiles: Vector[Tile]): Vector[Tile] =
    val occurrences = tiles.flatMap(_.edges).groupMapReduce(identity)(_ => 1)(_ + _)
    val categories  = tiles.groupBy(tile => tile.edges.count(edge => occurrences(edge) == 1))
    categories(4)

  val tiles: Vector[Tile] =
    input
      .trim.split("\n\n").toSeq
      .map(_.split("\n").map(_.trim))
      .map: tile =>
        val id     = tile.head.slice(5, 9).toLong
        val data   = tile.drop(1)
        val pixels = for y <- 0 to 9; x <- 0 to 9 yield (x, y) -> data(y)(x)
        Tile(id, Image(10, pixels.toMap))
      .toVector

  def solve1(tiles: Vector[Tile]): Long =
    corners(tiles).map(_.id).product

  def unscrambleTiles(tiles: Vector[Tile]): Map[Pos, Tile] =
    val size = math.sqrt(tiles.size).toInt
    val positions = for x <- 0 until size; y <- 0 until size yield (x, y)
    val corner = corners(tiles).head.permutations
    val occurrences = tiles.flatMap(_.edges).groupMapReduce(identity)(_ => 1)(_ + _)

    @tailrec
    def go(remaining: Vector[Tile], positions: Vector[Pos], ordered: Map[Pos, Tile]): Map[Pos, Tile] =
      if positions.isEmpty then
        ordered
      else
        val tile = positions.head match
          case (x = 0, y = 0) => corner.filter(tile => occurrences(tile.top) == 1 && occurrences(tile.left) == 1).head
          case (x = 0, y = y) => remaining.filter(tile => tile.top == ordered((0, y - 1)).bottom).head
          case (x = x, y = y) => remaining.filter(tile => tile.left == ordered((x - 1, y)).right).head
        go(remaining.filterNot(_.id == tile.id), positions.tail, ordered.updated(positions.head, tile))

    go(tiles.flatMap(_.permutations), positions.toVector, Map.empty)

  def assembleImage(unscrambled: Map[Pos, Tile]): Image =
    val size = 8 * math.sqrt(unscrambled.size).toInt
    val pixels = for
      x <- 0 until size
      y <- 0 until size
    yield
      (x, y) -> unscrambled((x / 8, y / 8)).image.pixels((1 + x % 8, 1 + y % 8))

    Image(size, pixels.toMap)

  def findMonsters(image: Image): Vector[Int] =
    for
      candidate <- image.permutations
    yield
      val matches = for
        x <- 0 until image.size - 20 // monster dimensions are 20 x 3
        y <- 0 until image.size - 3
        if monster.map(_ + (x, y)).forall(pos => candidate.pixels(pos) == '#')
      yield (x, y)
      val monsters = matches.map((_, _)).flatMap(pos => monster.map(_ + pos)).toSet
      candidate.pixels.keys.count(pos => candidate.pixels(pos) == '#' && !monsters.contains(pos))

  def solve2(tiles: Vector[Tile]): Long =
    val unscrambled   = unscrambleTiles(tiles)
    val completeImage = assembleImage(unscrambled)
    val roughness     = findMonsters(completeImage)
    roughness.min

  override lazy val answer1: Long = solve1(tiles)
  override lazy val answer2: Long = solve2(tiles)
