package examples

import org.scalatest.funsuite.AnyFunSuite

class TestFSM extends AnyFunSuite:

  test("FSM.compilation"):

    /** correct transition order */
    assertCompiles(
      """import FSM.*
        |val order1 = Order.make(item = "chair", price = 666.00)
        |val order2 = order1.pay      // (1)
        |val order3 = order2.ship     // (2)
        |val order4 = order3.deliver  // (3)
        |""".stripMargin)

    /** incorrect transition order */
    assertDoesNotCompile(
      """import FSM.*
        |val order1 = Order.make(item = "chair", price = 666.00)
        |val order2 = order1.pay      // (1)
        |val order3 = order2.deliver  // (3)
        |val order4 = order3.ship     // (2)
        |""".stripMargin)
