package aoc2015

import nmcb.*

object Day19 extends AoC:


  val molecules: String =
    lines
      .filterNot(_.isBlank)
      .filterNot(_.contains("=>"))
      .head

  val replacements: Vector[(String,String)] = lines.collect:
    case s"$molecule => $replacement" => molecule -> replacement

  override lazy val answer1: Int =
    var generations = Set.empty[String]
    for from -> to <- replacements do
      for index <- 0 until molecules.length do
        if molecules.slice(index, index + from.length) == from then
          val prefix  = molecules.substring(0, index)
          val postfix = molecules.substring(index + from.length)
          generations += (prefix + to + postfix)
    generations.size

  /** Luck > Skill : replacements are already in the right order */
  override lazy val answer2: Int =
    var target = molecules
    var count  = 0
    while target != "e" do
      for from -> to <- replacements do
        if target.contains(to) then
          target  = target.replaceFirst(to, from)
          count = count + 1
    count
