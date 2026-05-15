package aoc2021

import nmcb.*
import nmcb.pos.*

object Day20 extends AoC:

  case class Image(grid: Grid[Boolean], canvas: Boolean)

  val offsets: Seq[Pos] =
    for
      dy <- -1 to 1
      dx <- -1 to 1
    yield
      (dx, dy)

  def int(binary: Vector[Boolean]): Int =
    binary.foldLeft(0): (acc, bit) =>
      (acc << 1) | (if bit then 1 else 0)

  def enhance(algorithm: Vector[Boolean], image: Image): Image =
    val frame    = Vector.fill(image.grid.sizeX + 2)(image.canvas)
    val framed   = Grid.fromMatrix(frame +: image.grid.matrix.map(image.canvas +: _ :+ image.canvas) :+ frame)
    val enhanced = framed.mapElement: (pos, cell) =>
      val area = offsets.map(_ + pos)
      val bin  = area.map(cell => if framed.within(cell) then framed.peek(cell) else image.canvas)
      algorithm(int(bin.toVector))

    val Int9x0 = 0
    val Int9x1 = 511
    val background = algorithm(if image.canvas then Int9x1 else Int9x0)
    Image(enhanced, background)

  def parseImage(s: String): Image =
    Image(Grid.fromLines(s.linesIterator).map(_ == '#'), false)

  def parse(input: String): (Vector[Boolean], Image) =
    val Vector(algorithmStr, imageStr) = input.split("\n\n").toVector
    val algorithm = algorithmStr.map(_ == '#').toVector
    val image = parseImage(imageStr)
    (algorithm, image)

  val (algorithm: Vector[Boolean], image: Image) = parse(input)


  override lazy val answer1: Int = enhance(algorithm, image).grid.count(_ == true)
  override lazy val answer2: Int = (1 to 50).foldLeft(image)((img, _) => enhance(algorithm, img)).grid.count(_ == true)
