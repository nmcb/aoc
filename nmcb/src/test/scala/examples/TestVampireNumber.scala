package examples

import org.scalatest.funsuite.AnyFunSuite

class TestVampireNumber extends AnyFunSuite:

  /** [OEIS-A014575](https://oeis.org/A014575) */
  test("VampireNumber") {
    println(VampireNumber.sieve(135837))
    assertResult(
      Vector(
        1260, 1395, 1435, 1530, 1827, 2187, 6880, 102510, 104260, 105210, 105264, 105750, 108135,
        110758, 115672, 116725, 117067, 118440, 120600, 123354, 124483, 125248, 125433, 125460,
        125500, 126027, 126846, 129640, 129775, 131242, 132430, 133245, 134725, 135828, 135837
      )
    )(VampireNumber.sieve(135837))
  }
