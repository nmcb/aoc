package nmcb

object BronKerbosch:

  /** Computes the maximum clique, i.e. the largest group of neighbouring nodes. */
  def maximumClique[A](graph: Map[A, Set[A]]): Set[A] =
    var maximum: Set[A] = Set.empty

    def loop(r: Set[A], p: Set[A], x: Set[A]): Unit =
      if p.isEmpty && x.isEmpty then
        if r.size > maximum.size then maximum = r
      else
        val u = (p union x).maxBy(node => graph.getOrElse(node, Set.empty).size)

        var bp = p
        var bx = x

        val candidates = p diff graph.getOrElse(u, Set.empty)

        for v <- candidates do
          val neighbors = graph.getOrElse(v, Set.empty)
          loop(r + v, bp intersect neighbors, bx intersect neighbors)
          bp -= v
          bx += v

    loop(Set.empty, graph.keySet, Set.empty)
    maximum

