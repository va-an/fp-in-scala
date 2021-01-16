import io.vaan.datatype.Cat
import cats.Semigroupal
import cats.implicits._

// type class that allows us to combine contexts
val fromProduct = Semigroupal[Option].product(4.some, "foo".some)
val fromTuple2 = Semigroupal.tuple2(4.some, "foo".some)

// the same
fromProduct === fromTuple2

// if one of None - entire is None
Semigroupal[Option].product(
  none[String],
  4.some
)

Semigroupal[Option].product(
  4.some,
  none[String]
)

// for more than 2 values
Semigroupal.tuple3(
  1.some,
  2.some,
  3.some
)

Semigroupal.tuple4(
  1.some,
  2.some, 
  none[Int],
  4.some
)

Semigroupal.map2(3.some, 4.some)((x, y) => x min y)
Semigroupal.map3(3.some, 4.some, 5.some)(_ + _ + _)

// Apply Syntax for Semigroupal
(3.some, 4.some).tupled
(3.some, none[Int]).tupled

val fromTupled = (4.some, "foo".some).tupled
fromTupled === fromProduct && fromTupled === fromProduct

// each value in some context
val maybeCat = (
  "Barsik".some,
  4.some,
  "black".some
).mapN(Cat.apply)

// if any of values failed, mapN returns "failed" context
val maybeNot = (
  "Barsik".some,
  none[Int],
  "black".some
).mapN(Cat.apply)

