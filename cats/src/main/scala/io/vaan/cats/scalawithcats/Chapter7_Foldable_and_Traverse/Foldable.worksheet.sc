import cats.Foldable
import cats.implicits._

val xs = List(1, 2, 3)

// for List
Foldable[List].foldLeft(xs, 0)(_ + _)

// for Option
Foldable[Option].foldLeft(4.some, 5)(_ * _)
Foldable[Option].foldLeft(none[Int], 5)(_ * _)

// some methods
Foldable[Option].nonEmpty(4.some)
Foldable[List].find(xs)(x => x == 1)
Foldable[List].exists(xs)(x => x == 1)

// monoids ops
Foldable[List].combineAll(xs)
Foldable[List].foldMap(xs)(_.toString)

// compose Foldables
val ys = List(
  List(1, 2),
  List(3, 4)
)

(Foldable[List] compose Foldable[List]).combineAll(ys)

// syntax for Foldable
val zx = List(1, 2, 3)

zx.combineAll
zx.foldMap(x => s"value $x; ")
