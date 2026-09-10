package examples

object FSM:

  import Order.*
  import Order.StateValue.*


  object Order:

    sealed trait StateType
    trait Placed    extends StateType
    trait Paid      extends StateType
    trait Shipped   extends StateType
    trait Delivered extends StateType

    enum StateValue derives CanEqual:
      case PLACED
      case PAID
      case SHIPPED
      case DELIVERED

    import StateValue.*

    def make(item: String, price: Double): Order[Placed] =
      Order[Placed](item = item, price = price, state = PLACED)


  case class Order[State <: StateType](item: String, price: Double, state: StateValue):

    def pay(using ev: State =:= Placed): Order[Paid] =
      copy[Paid](state = PAID)

    def ship(using ev: State =:= Paid): Order[Shipped] =
      copy[Shipped](state = SHIPPED)

    def deliver(using ev: State =:= Shipped): Order[Delivered] =
      copy[Delivered](state = DELIVERED)


  object Client:

    private val order1 = Order.make(item = "chair", price = 666.00)
    private val order2 = order1.pay
    private val order3 = order2.ship
    private val order4 = order3.deliver

