import cats.Eq
import cats.implicits._
import io.vaan.datatype.Cat

// programmer error, but not type error
List(1, 2, 3).map(_.some).filter(_ == 1)

val eqInt: Eq[Int] = Eq[Int]
eqInt.eqv(2, 3)

// Eq for Int
2 === 3
2 =!= 3

// Eq for Option
2.some === 3.some
2.some === none[Int]

// Eq for Custom Type (Meow)
val barsik = Cat("Barsik", 4, "white")
val murzik = Cat("Murzik", 5, "black")

implicit val catEq: Eq[Cat] =
  Eq.instance[Cat] { (cat1, cat2) =>
    cat1 == cat2
  }

barsik === murzik
barsik === Cat("Barsik", 4, "white")

barsik.some === murzik.some
barsik.some === none[Cat]

barsik == murzik.some // false
//barsik === murzik.some // not compile
