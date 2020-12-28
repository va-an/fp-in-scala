// function composition
val f1: Int => Int = (x: Int) => x * x
val f2: Int => Int = (x: Int) => x + x

f1(f2(4))
(f1 compose f2)(4)
(f2 andThen f1)(4)
// (f map f2)(4) // FIXME: why don't works?

import cats.Functor
import cats.implicits._

val xs = List(1, 2, 3)

Functor[List].map(xs)(_ * 2)
Functor[Option].map(4.some)(x => x * x)

val f1Lifted = Functor[Option].lift(f1)
f1Lifted(4.some)

// calculate value in context
def calculate[F[_]](value: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(value)(x => x * x)

calculate(List(1, 2, 3))
calculate(4.some)

// more abstract
def calculateByFormula[F[_], A, B](value: F[A])(f: A => B)(implicit functor: Functor[F]) =
    functor.map(value)(f)

calculateByFormula(List(1, 2, 3))(x => x * x)
calculateByFormula(4.some)(_ + 4)
calculateByFormula(Option(4.5))(_ * 2)

final case class Box[A](value: A)

implicit val boxFunctor: Functor[Box] = 
    new Functor[Box] {
        override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))
    }

val box = Box[Int](123)
box.map(value => value + 1)

Box("hello").map(x => s"$x $x")
