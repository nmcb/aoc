package examples

import scala.annotation.tailrec

object Dependencies extends App:

  type Name  = String
  type Uri   = String
  type Value = Int | List[Int]
  type Json  = String

  case class Service(uri: Uri, dependencies: Set[Name], mock: Value):

    def call(json: Json): Value =
      println(s"called uri=$uri with $json returning value=$mock")
      mock


  extension (nameValue: (Name,Value))

    def toJson: Json =
      nameValue match
        case (name, int: Int)        => s"\"$name\":$int"
        case (name, list: List[Int]) => s"\"$name\":[${list.mkString(",")}]"


  extension (parameters: Map[Name,Value])

    def toJson: Json =
      parameters.map(_.toJson).mkString("{", ",", "}")


  extension (input: Map[Name,Service])

    def dependencies: Map[Name,Set[Name]] =
      input.map((name, service) => name -> service.dependencies)

    def serviceFor(name: Name): Service =
      input.getOrElse(name, sys.error(s"no service configured for name: $name"))

    def dependenciesFor(name: Name): Set[Name] =
      dependencies.getOrElse(name, Set.empty)

    def isCyclicFor(request: Set[Name]): Boolean =
      def loop(name: Name, visited: Set[Name] = Set.empty[Name]): Boolean =
        if visited.contains(name) then
          true
        else
          dependenciesFor(name).exists: dependency =>
            if input.contains(dependency) then
              loop(dependency, visited + name)
            else
              false
      request.exists(r => loop(r))

    def narrowFor(request: Set[Name], event: Set[Name]): Map[Name,Service] =
      @tailrec
      def loop(todo: Set[Name], result: Set[Name] = Set.empty): Set[Name] =
        if todo.isEmpty then
          result
        else
          val found = result ++ (todo diff event)
          val next  = found.flatMap(dependenciesFor) -- found -- event
          loop(next, found)
      loop(request).map(name => name -> serviceFor(name)).toMap

    def sequenceFor(event: Set[Name]): Vector[Name] =
      @tailrec
      def loop(todo: Map[Name,Set[Name]], result: Vector[Name] = Vector.empty): Vector[Name] =
        val (leafs, branches) = todo.partition((name, dependencies) => (dependencies -- event).isEmpty)
        if leafs.isEmpty then
          result
        else
          val purged = branches.map((name, dependencies) => name -> (dependencies -- leafs.keySet))
          loop(purged, result :++ leafs.keySet)
      loop(input.dependencies)

    def resolve(request: Set[Name], event: Map[Name,Value]): Map[Name,Value] =
      if isCyclicFor(request) then
        sys.error(s"cyclic request for names: ${request.mkString(",")}")
      else
        narrowFor(request, event.keySet)
          .sequenceFor(event.keySet)
          .foldLeft(event): (result, name) =>
            val parameters = result.filter((found, _) => dependenciesFor(name).contains(found))
            val response   = serviceFor(name).call(parameters.toJson)
            result + (name -> response)
          .filter((n,v) => request.contains(n))


  val input: Map[Name,Service] = Map(

    // some dependency block note "q" should be given in the event
    "a" -> Service(uri = "http://x1/", mock = 100, dependencies = Set("b", "c", "q")),
    "b" -> Service(uri = "http://x2/", mock = List(101,102), dependencies = Set("c", "d")),
    "c" -> Service(uri = "http://y1/", mock = 103, dependencies = Set("e")),
    "d" -> Service(uri = "http://y2/", mock = 104, dependencies = Set("q")),
    "e" -> Service(uri = "http://z1/", mock = 105, dependencies = Set("d", "m")),

    // former depends on this block via the last service
    "m" -> Service(uri = "http://q1/", mock = List(200,201), dependencies = Set("n")),
    "n" -> Service(uri = "http://q2/", mock = 203, dependencies = Set("q")),

    // cycle but never requested
    "o" -> Service(uri = "http://r1/", mock = 300, dependencies = Set("p")),
    "p" -> Service(uri = "http://r2/", mock = 301, dependencies = Set("o")),

  )
  println(s"input=\n${input.mkString("\n")}")

  val request = Set("a", "b", "c")
  println(s"request=$request")

  val event = Map("q" -> 666)
  println(s"event=${event.toJson}")

  val result = input.resolve(request, event)
  println(s"result=${result.toJson}")
