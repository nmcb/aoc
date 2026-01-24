package aoc2019

import nmcb.*
object Day07 extends AoC:

  import cpu.*

  val program: Mem = Mem.parse(input)

  def highestSignal(program: Mem, phaseSettings: Range): Value =
    phaseSettings
      .permutations
      .map(_.foldLeft(0L): (total,setting) =>
        CPU(program).withInput(setting,total).outputs.last)
      .max

  def highestSignalWithFeedback(program: Mem, phaseSettings: Range): Value =

    def signalWithFeedback(settings: Vector[Value]): Value =
      lazy val outputs: LazyList[Value] =
        settings.foldLeft(0L #:: outputs): (inputs, setting) =>
          CPU(program, setting #:: inputs).outputs

      outputs.last

    phaseSettings
      .map(_.toLong)
      .toVector
      .permutations
      .map(signalWithFeedback)
      .max

  override lazy val answer1: Value = highestSignal(program, 0 to 4)
  override lazy val answer2: Value = highestSignalWithFeedback(program, 5 to 9)
