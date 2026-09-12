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

    enum StateValue[State] derives CanEqual:
      case PLACED    extends StateValue[Placed]
      case PAID      extends StateValue[Paid]
      case SHIPPED   extends StateValue[Shipped]
      case DELIVERED extends StateValue[Delivered]

    import StateValue.*

    def make(item: String, price: Double): Order[Placed] =
      Order(item = item, price = price, state = PLACED)


  case class Order[State <: StateType] private (item: String, price: Double, state: StateValue[State]):

    private def advance[Next <: StateType](next: StateValue[Next]): Order[Next] =
      copy(state = next)

    def pay(using ev: State =:= Placed): Order[Paid] =
      advance(PAID)

    def ship(using ev: State =:= Paid): Order[Shipped] =
      advance(SHIPPED)

    def deliver(using ev: State =:= Shipped): Order[Delivered] =
      advance(DELIVERED)


  object Client:

    private val order1 = Order.make(item = "chair", price = 666.00)
    private val order2 = order1.pay
    private val order3 = order2.ship
    private val order4 = order3.deliver
