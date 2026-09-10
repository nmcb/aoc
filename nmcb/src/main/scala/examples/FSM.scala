package examples

object FSM:

  sealed trait OrderState
  trait Placed    extends OrderState
  trait Paid      extends OrderState
  trait Shipped   extends OrderState
  trait Delivered extends OrderState

  case class Order[State <: OrderState](item: String, price: Double, state: String):

    def pay(using ev: State =:= Placed): Order[Paid] =
      copy[Paid](state = "paid")

    def ship(using ev: State =:= Paid): Order[Shipped] =
      copy[Shipped](state = "shipped")

    def deliver(using ev: State =:= Shipped): Order[Delivered] =
      copy[Delivered](state = "delivered")


  object Order:

    def make[State <: OrderState](item: String, price: Double, state: String = "placed"): Order[State] =
      state match
        case "placed"    => Order[State](item = item, price = price, state = state)
        case "paid"      => Order[State](item = item, price = price, state = state)
        case "shipped"   => Order[State](item = item, price = price, state = state)
        case "delivered" => Order[State](item = item, price = price, state = state)
        case unknown     => sys.error(s"unknow order state: $unknown")

  object Client:

    val order1: Order[Placed]    = Order.make(item = "chair", price = 666.00)
    val order2: Order[Paid]      = order1.pay
    val order3: Order[Shipped]   = order2.ship
    val order4: Order[Delivered] = order3.deliver

