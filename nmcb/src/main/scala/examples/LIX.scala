package examples

import scala.util.matching.Regex

object LIX:

  val WORDS: Regex     = """\b[a-zA-Z]+(?:-[a-zA-Z]+)*\b""".r
  val SENTENCES: Regex = "[A-Z]\\.:".r

  def calculate(text: String): Double =
    if text.trim.isEmpty then
      0.0
    else
      val sentences     = text.trim.split("[.!?]+").filter(_.trim.nonEmpty).toVector
      val sentenceCount = sentences.size
      if sentenceCount == 0 then
        0.0
      else
        val words     = text.trim.split("\\s+").map(_.replaceAll("[^a-zA-Z]", "")).filter(_.nonEmpty).toVector
        val wordCount = words.size
        if wordCount == 0 then
          0.0
        else
          val longWordCount         = words.count(_.length > 6)
          val averageSentenceLength = wordCount.toDouble / sentenceCount
          val percentageLongWords   = longWordCount.toDouble * 100 / wordCount
          BigDecimal(averageSentenceLength + percentageLongWords).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
