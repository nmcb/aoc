package aoc2021

import nmcb.*
import nmcb.predef.*
import nmcb.pos.*

object Day20 extends AoC:

  case class Image(algorithm: Vector[Boolean], grid: Grid[Boolean], canvas: Boolean):

    import Image.*

    extension (binary: Seq[Boolean])
      def toInt: Int =
        binary.foldLeft(0): (acc, bit) =>
          (acc << 1) | (if bit then 1 else 0)

    def enhance: Image =
      val frame  = Vector.fill(grid.sizeX + 2)(canvas)
      val framed = Grid.fromMatrix(frame +: grid.matrix.map(canvas +: _ :+ canvas) :+ frame)
      val enhanced = framed.mapElement: (pos, _) =>
        val area   = offsets.map(_ + pos)
        val binary = area.map(cell => if framed.within(cell) then framed.peek(cell) else canvas)
        algorithm(binary.toInt)

      val Int9x0 = 0
      val Int9x1 = 511
      val background = algorithm(if canvas then Int9x1 else Int9x0)
      copy(grid = enhanced, canvas = background)

  object Image:

    def fromString(s: String): Image =
      val Array(as, is) = s.split("\n\n")
      val algorithm = as.map(_ == '#').toVector
      val grid      = Grid.fromLines(is.linesIterator).map(_ == '#')
      Image(algorithm, grid, false)

    val offsets: Seq[Pos] =
      for
        dy <- -1 to 1
        dx <- -1 to 1
      yield
        (dx, dy)

  val image: Image = Image.fromString(input)

  override lazy val answer1: Int = image.enhance.grid.count(_ == true)
  override lazy val answer2: Int = Iterator.iterate(image)(_.enhance).nth(50).grid.count(_ == true)
