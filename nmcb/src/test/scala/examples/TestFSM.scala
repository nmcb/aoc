package examples

import org.scalatest.funsuite.AnyFunSuite

class TestFSM extends AnyFunSuite:

  test("FSM.compilation"):

    /** correct transition order */
    assertCompiles(
      """import FSM.*
        |val order1 = Order.make(item = "chair", price = 666.00)
        |val order2 = order1.pay      // (1) Compiles
        |val order3 = order2.ship     // (2) Compiles
        |val order4 = order3.deliver  // (3) Compiles
        |""".stripMargin)

    /** incorrect transition order */
    assertDoesNotCompile(
      """import FSM.*
        |val order1 = Order.make(item = "chair", price = 666.00)
        |val order2 = order1.pay      // (1) Compiles
        |val order3 = order2.deliver  // (3) ERROR: Cannot prove that examples.FSM.Order.Paid =:= examples.FSM.Order.Shipped.
        |val order4 = order3.ship     // (2)
        |""".stripMargin)
