import cats.Monad
import cats.implicits._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

// monad from Cats
val opt1 = Monad[Option].pure(4)
val opt2 = Monad[Option].flatMap(opt1)(x => (x * x).some)
val opt3 = Monad[Option].map(opt2)(x => x / 2)

Monad[List].fmap((List(1, 2, 3)))(x => x * 2)

// monad for future
import scala.concurrent.ExecutionContext.Implicits.global
val fm = Monad[Future]

val future = fm.flatMap(fm.pure(4))(x => fm.pure(x * 2))
Await.result(future, 1 second)

// monad syntax
1.pure[Option]
1.pure[List] == Monad[List].pure(1)

// monads, son
def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(aValue => 
        b.map(bValue => 
            aValue * aValue + bValue * bValue
        )
    )

sumSquare(3.some, 4.some)
sumSquare(List(1, 2, 3), List(3, 4))
