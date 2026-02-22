package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day20 extends AoC:

  enum Pulse derives CanEqual:
    case High
    case Low

  import Pulse.*
  
  type Name = String
  type Message = (from: Name, to: Name, pulse: Pulse)

  sealed trait Module:
    def name: Name
    def destinations: Vector[Name]
    def receive(from: Name, pulse: Pulse): (Vector[Message], Module)


  case class FlipFlop(name: Name, destinations: Vector[Name], on: Boolean = false) extends Module:

    override def receive(from: Name, pulse: Pulse): (Vector[Message], Module) =
      pulse match
        case High => (Vector.empty, this)
        case Low  => (destinations.map((name, _, if on then Low else High)), copy(on = !on))


  case class Conjunction(name: Name, destinations: Vector[Name], inputs: Map[Name, Pulse] = Map.empty) extends Module:

    def withInputs(modules: Vector[Module]): Conjunction =
      copy(inputs = modules.map(_.name -> Low).toMap)

    def receive(from: Name, pulse: Pulse): (Vector[Message], Module) =
      val next = inputs.updated(from, pulse)
      val sent = if next.view.values.forall(_ == High) then Low else High
      (destinations.map((name, _, sent)), copy(inputs = next))


  case class Broadcaster(name: Name, destinations: Vector[Name]) extends Module:

    def receive(from: Name, pulse: Pulse): (Vector[Message], Module) =
      (destinations.map((name, _, pulse)), this)


  case class Machine(modules: Map[Name, Module], sent: Vector[Message] = Vector.empty):

    def sent(message: Message): (Vector[Message], Machine) =
      modules.get(message.to) match
        case Some(module) =>
          val (schedule, next) = module.receive(message.from, message.pulse)
          (schedule, copy(modules = modules.updated(message.to, next), sent = sent :+ message))
        case None =>
          (Vector.empty, copy(sent = sent :+ message))

    def press: Machine =
      process(Vector(("button", "broadcaster", Low)), this)

    @tailrec
    final def process(messages: Vector[Message], state: Machine): Machine =
      if messages.isEmpty then
        state
      else
        val (schedule, next)  = state.sent(messages.head)
        process(messages.tail ++ schedule, next)

    def result: Long =
      sent.count(_.pulse == Low) * sent.count(_.pulse == High)

    def gcd(l: Long, r: Long): Long = if r == 0 then l.abs else gcd(r, l % r)
    def lcm(l: Long, r: Long): Long = (l * r).abs / gcd(l, r)
    def lcm(ls: Iterable[Long]): Long = ls.reduce(lcm)

    /** the specification shows one '&' outputting to 'rx', we solve for the cycles of its inputs being raised to 'H' */
    def solveRX: Long =
      import collection.mutable

      var machine = this
      val input   = machine.modules.filter((n,m) => m.destinations.contains("rx")).map((n,m) => n).head
      val todo    = machine.modules.filter((n,m) => m.destinations.contains(input)).map((n,m) => n).to(mutable.Set)
      val found   = mutable.Map.empty[String, Int]
      var pressed = 0

      while todo.nonEmpty do
        machine = machine.press
        pressed += 1
        for
          case (from, _, High) <- machine.sent
          if todo(from)
        yield
          found(from) = pressed
          todo -= from

      lcm(found.view.values.map(_.toLong))


  val machine: Machine =

    val parsed: Vector[Module] =
      lines
        .map:
          case s"%$name -> $destinations" => FlipFlop(name, destinations.split(", ").toVector)
          case s"&$name -> $destinations" => Conjunction(name, destinations.split(", ").toVector)
          case  s"$name -> $destinations" => Broadcaster(name, destinations.split(", ").toVector)

    val modules: Map[Name, Module] =
      parsed
        .map:
          case m: Conjunction => m.name -> m.withInputs(parsed.filter(_.destinations.contains(m.name)))
          case m              => m.name -> m
        .toMap

    Machine(modules)

  override lazy val answer1: Long = (1 to 1000).foldLeft(machine)((m, _) => m.press).result
  override lazy val answer2: Long = machine.solveRX
