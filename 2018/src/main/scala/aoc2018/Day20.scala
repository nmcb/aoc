package aoc2018

import nmcb.*
import scala.annotation.tailrec

object Day20 extends AoC:

  case class Location(x: Int, y: Int):
    def move(direction: Char): Location =
      direction match
        case 'N' => copy(y = y - 1)
        case 'E' => copy(x = x - 1)
        case 'S' => copy(y = y + 1)
        case 'W' => copy(x = x + 1)

  def solve(directions: String): Vector[Int] =

    type MaxDoorsTo = (Location,Int)
    type Visited = Set[Location]

    @tailrec
    def go(todo: String, maxDoorsTo: Map[Location,Int], visited: Visited, branches: Vector[Visited]): Vector[Int] =
      todo.head match
        case '^' =>
          go(
            todo       = todo.tail,
            maxDoorsTo = Map(Location(0,0) -> 0),
            visited    = Set(Location(0,0)),
            branches   = Vector.empty
          )
        case '$' =>
          maxDoorsTo.values.toVector
        case '(' =>
          go(
            todo       = todo.tail,
            maxDoorsTo = maxDoorsTo,
            visited    = visited,
            branches   = Set.empty +: visited +: branches
          )
        case '|' =>
          val accumulated +: previous +: trail = branches: @unchecked
          go(
            todo       = todo.tail,
            maxDoorsTo = maxDoorsTo,
            visited    = previous,
            branches   = (accumulated ++ visited) +: previous +: trail
          )
        case ')' =>
          val accumulated +: previous +: trail = branches: @unchecked
          go(
            todo       = todo.tail,
            maxDoorsTo = maxDoorsTo,
            visited    = accumulated ++ visited,
            branches   = trail
          )
        case direction =>
          val (nextMaxDoorsTo, nextVisited) =
            visited.foldLeft((maxDoorsTo, Set.empty[Location])):
              case ((maxDoorsTo, visited), currentRoom) =>
                val nextRoom = currentRoom.move(direction)
                if maxDoorsTo.contains(nextRoom) && maxDoorsTo(nextRoom) <= maxDoorsTo(currentRoom) + 1 then
                  (maxDoorsTo, visited + nextRoom)
                else
                  (maxDoorsTo.updated(nextRoom, maxDoorsTo(currentRoom) + 1), visited + nextRoom)
          go(
            todo       = todo.tail,
            maxDoorsTo = nextMaxDoorsTo,
            visited    = nextVisited,
            branches   = branches
          )
    go(
      todo        = directions,
      maxDoorsTo  = Map.empty,
      visited     = Set.empty,
      branches    = Vector.empty
    )

  override lazy val answer1: Int = solve(input).max
  override lazy val answer2: Int = solve(input).count(_ >= 1000)
