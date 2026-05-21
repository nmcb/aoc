package aoc2021

import nmcb.*

/** @see Credits - https://github.com/maneatingape */
object Day23 extends AoC:

  val spaceCost: Map[Char, Int] = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val roomIndex: Map[Char, Int] = Map('A' -> 2, 'B' ->  4, 'C' ->   6, 'D' ->    8)

  case class Estate(roomMax: Int, cost: Int, hallway: Vector[Char], rooms: Map[Char, Vector[Char]]):

    def finished: Boolean =
      rooms.forall((kind, room) => room.size == roomMax && room.forall(_ == kind))

  def parsePart1(input: Vector[String]): Estate =

    def parse(column: Int): Vector[Char] =
      Vector(input(2)(column), input(3)(column))

    Estate(
      roomMax = 2,
      cost    = 0,
      hallway = Vector.fill(11)('.'),
      rooms   = Map(
        'A' -> parse(3),
        'B' -> parse(5),
        'C' -> parse(7),
        'D' -> parse(9)
      )
    )

  def parsePart2(input: Vector[String]): Estate =

    def parse(column: Int, second: Char, third: Char): Vector[Char] =
      Vector(input(2)(column), second, third, input(3)(column))

    Estate(
      roomMax = 4,
      cost    = 0,
      hallway = Vector.fill(11)('.'),
      rooms   = Map(
        'A' -> parse(3, 'D', 'D'),
        'B' -> parse(5, 'C', 'B'),
        'C' -> parse(7, 'B', 'A'),
        'D' -> parse(9, 'A', 'C')
      )
    )

  def paths(estate: Estate): Vector[Estate] =

    val outside   = estate.hallway.zipWithIndex.filter((amphipod,_) => amphipod != '.')
    val first     = outside.flatMap(hallwayToRoom(estate))
    val second    = estate.rooms.flatMap(roomToRoom(estate))
    val preferred = first ++ second

    if preferred.nonEmpty then
      preferred
    else
      estate.rooms.flatMap(roomToHallway(estate)).toVector

  def hallwayToRoom(current: Estate)(amphipod: Char, start: Int): Option[Estate] =

    val end   = roomIndex(amphipod)
    val range = if start < end then (start + 1) to end else end until start

    if current.rooms(amphipod).forall(_ == amphipod) && range.forall(n => current.hallway(n) == '.') then
      val hallway = current.hallway.updated(start, '.')
      val rooms   = current.rooms.updated(amphipod, current.rooms(amphipod).prepended(amphipod))
      val cost    = current.cost + (range.size + current.roomMax - current.rooms(amphipod).size) * spaceCost(amphipod)
      Some(Estate(current.roomMax, cost, hallway, rooms))
    else None

  def roomToRoom(current: Estate)(key: Char, room: Vector[Char]): Vector[Estate] =
    if room.forall(_ == key) then return Vector.empty

    val start = roomIndex(key)
    val end   = roomIndex(room.head)
    val range = if start < end then start to end else end to start

    if current.rooms(room.head).forall(_ == room.head) && range.forall(n => current.hallway(n) == '.') then
      val rooms = current.rooms
        .updated(key, room.tail)
        .updated(room.head, current.rooms(room.head).prepended(room.head))
      val cost = current.cost + (range.size + current.roomMax - current.rooms(key).size + current.roomMax - current.rooms(room.head).size) * spaceCost(room.head)
      Vector(Estate(current.roomMax, cost, current.hallway, rooms))
    else Vector.empty

  def roomToHallway(current: Estate)(key: Char, room: Vector[Char]): Vector[Estate] =
    if room.forall(_ == key) then return Vector.empty

    val index = roomIndex(key)
    val valid = Vector(0, 1, 3, 5, 7, 9, 10)
    val left  = valid.filter(_ < index).reverse.takeWhile(n => current.hallway(n) == '.')
    val right = valid.filter(_ > index).takeWhile(n => current.hallway(n) == '.')

    (left.reverse ++ right)
      .map: pos =>
        val hallway = current.hallway.updated(pos, room.head)
        val rooms   = current.rooms.updated(key, room.tail)
        val cost    = current.cost + ((pos - index).abs + 1 + current.roomMax - room.size) * spaceCost(room.head)
        Estate(current.roomMax, cost, hallway, rooms)

  def shuffle(burrow: Estate): Int =

    def move(current: Estate, energy: Option[Int]): Option[Int] =
      if current.finished then
        Some(current.cost)
      else if energy.exists(_ < current.cost) then
        None
      else
        paths(current).flatMap(move(_,energy)).minOption

    move(burrow, None).getOrElse(sys.error(s"not found"))


  override lazy val answer1: Int = shuffle(parsePart1(lines))
  override lazy val answer2: Int = shuffle(parsePart2(lines))
