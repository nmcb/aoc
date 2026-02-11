package nmcb

import nmcb.*
import nmcb.pos.{*, given}
import nmcb.Dijkstra.*
import org.scalatest.funsuite.AnyFunSuite

val input =
  """.###...
    |.##..#.
    |....#..
    |...#..#
    |..#..#.
    |..#.#..
    |#.#....
    """.stripMargin

val shortest =
  """O###OOO
    |O##OO#O
    |OOOO#OO
    |...#OO#
    |..#OO#.
    |..#O#..
    |#.#OOOO
    |""".stripMargin.trim

class TestDijkstra extends AnyFunSuite:

  test("Dijkstra.run") {
    val grid   = Grid.fromString(input)
    val graph  = Graph.fromGrid(grid, '.')
    val result = Dijkstra.run[Pos](grid.minPos, graph)
    val output = result.pathTo(grid.maxPos).toTrail.foldLeft(grid)(_.updated(_, 'O')).asString

    assertResult(expected = shortest)(output)
  }

  test("Dijkstra.reachable") {
    val grid      = Grid.fromString(input)
    val cluster   = grid.findAll('.')
    val reachable = Dijkstra.reachable[Pos](grid.minPos, _.adjWithinGrid(grid, _.element == '.'))

    assertResult(expected = cluster)(reachable)
  }