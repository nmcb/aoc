package examples

import org.scalatest.funsuite.AnyFunSuite

class TestLIX extends AnyFunSuite:

  test("LIX.calculate"):
    assertResult(91.92)(
      LIX.calculate(
        """The international scientific community continuously develops innovative solutions to modern global
          |challenges. Technological advancements frequently accelerate industrial efficiency across multiple
          |sectors simultaneously. However, implementation requires substantial financial investments and
          |comprehensive regulatory frameworks.
          |""".stripMargin)
    )

    assertResult(69.99)(
      LIX.calculate(
        """Climate change represents one of the most critical challenges of our modern era. Global temperatures
          |continue to rise at an unprecedented pace, driven primarily by the relentless accumulation of greenhouse
          |gases in the atmosphere. Scientists worldwide have documented widespread disruptions across diverse
          |ecosystems, including accelerated melting of polar ice caps, significant sea-level rise, and an
          |increased frequency of extreme weather events such as prolonged droughts and catastrophic floods.
          |Addressing this planetary crisis requires immediate, coordinated international cooperation and a massive
          |transition toward renewable energy sources. Governments, corporations, and individuals must actively
          |reduce their carbon footprints to secure a sustainable future for coming generations. Without decisive
          |policy interventions and technological innovations, environmental degradation will intensify, threatening
          |global food security, economic stability, and human health on a massive scale.
          |""".stripMargin)
    )
