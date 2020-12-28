import cats.{Monoid, Semigroup}
import cats.implicits._

// monoid for strings
Monoid[String].combine("Hello, ", "cats!")
Monoid[String].empty

// semigroup for string
Semigroup[String].combine("Hello, ", "cats!")

// monoid for Option[Int]
Monoid[Option[Int]].combine(1.some, 2. some)

// monoid syntax
1 |+| 2 |+| Monoid.empty[Int]
"Hello, " |+| "cats!"

// 2.5.4 Exercise: Adding All The Things
def addItems(items: List[Int]): Int =
    items.fold(Monoid[Int].empty)(Monoid[Int].combine)

addItems(List(1, 2, 3))

// with implicit val
def addImlicitVersion[A](xs: List[A])(implicit m: Monoid[A]): A =
    xs.fold(m.empty)(m.combine)

val xs = List(1.some, 2.some, None, 3.some)

addImlicitVersion(xs)

// context bound syntax
def add[A: Monoid](xs: List[A]): A = 
    xs.fold(Monoid.empty[A])(Monoid.combine)

add(xs)

case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid = Monoid.instance(
    emptyValue = Order(totalCost = 0, quantity = 0),

    cmb = (x: Order, y: Order) => Order(
        totalCost = x.totalCost + y.totalCost,
        quantity = x.quantity + y.quantity
    )
)

val orders = List(
    Order(100, 4),
    Order(200, 8),
    Order(150, 3)
)

add(orders)
