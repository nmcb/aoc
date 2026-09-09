package examples

import org.scalatest.funsuite.AnyFunSuite

class TestCatalanNumber extends AnyFunSuite:

  /** [OEIS-A000108](https://oeis.org/A000108) */
  test("CatalanNumber") {
    assertResult(
      Vector(
        1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845, 35357670,
        129644790, 477638700, 1767263190, 6564120420L, 24466267020L, 91482563640L, 343059613650L, 1289904147324L,
        4861946401452L, 18367353072152L, 69533550916004L, 263747951750360L, 1002242216651368L, 3814986502092304L
      )
    )(CatalanNumber.sieve(3814986502092305L))
  }
