package aoc2021

import nmcb.*
import nmcb.pos.*

object Day20 extends AoC:

  case class Image(pixels: Grid[Boolean], canvas: Boolean)

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
    val Image(pixels, canvas) = image
    val frame    = Vector.fill(pixels.sizeX + 2)(canvas)
    val framed   = Grid.fromMatrix(frame +: pixels.matrix.map(canvas +: _ :+ canvas) :+ frame)
    val enhanced = framed.mapElement: (pos, cell) =>
      val area = offsets.map(_ + pos)
      val bin  = area.map(cell => if framed.within(cell) then framed.peek(cell) else canvas)
      algorithm(int(bin.toVector))

    val Int9x0 = 0
    val Int9x1 = 511
    val background = algorithm(if canvas then Int9x1 else Int9x0)
    Image(enhanced, background)

  def parseImage(s: String): Image =
    Image(Grid.fromLines(s.linesIterator).map(_ == '#'), false)

  def parse(input: String): (Vector[Boolean], Image) =
    val Vector(algorithmStr, imageStr) = input.split("\n\n").toVector
    val algorithm = algorithmStr.map(_ == '#').toVector
    val image = parseImage(imageStr)
    (algorithm, image)

  val (algorithm: Vector[Boolean], image0: Image) = parse(input)


  override lazy val answer1: Int = enhance(algorithm, image0).pixels.count(_ == true)
  override lazy val answer2: Int = (1 to 50).foldLeft(image0)((img, _) => enhance(algorithm, img)).pixels.count(_ == true)
