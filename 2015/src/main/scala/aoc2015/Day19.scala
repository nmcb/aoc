package aoc2015

import nmcb.*

object Day19 extends AoC:

  type Molecule = String

  val molecule: Molecule =
    lines.filterNot(_.isBlank).filterNot(_.contains("=>")).head

  val replacements: Vector[(Molecule, Molecule)] = lines.collect:
    case s"$molecule => $replacement" => molecule -> replacement

  def solve1(molecules: Molecule, replacements: Vector[(Molecule, Molecule)]): Int =
    var generations = Set.empty[Molecule]
    for from -> to <- replacements do
      for index <- 0 until molecules.length do
        if molecules.slice(index, index + from.length) == from then
          val prefix  = molecules.substring(0, index)
          val postfix = molecules.substring(index + from.length)
          generations += (prefix + to + postfix)
    generations.size


  /** Luck > Skill : replacements are already in the right order */
  def solve2(molecule: Molecule, replacements: Vector[(Molecule, Molecule)]): Int =
    var target = molecule
    var count  = 0
    while target != "e" do
      for from -> to <- replacements do
        if target.contains(to) then
          target  = target.replaceFirst(to, from)
          count = count + 1
    count

  override lazy val answer1: Int = solve1(molecule, replacements)
  override lazy val answer2: Int = solve2(molecule, replacements)
