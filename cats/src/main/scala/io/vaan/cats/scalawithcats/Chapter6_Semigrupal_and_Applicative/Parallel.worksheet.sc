import cats.Semigroupal
import cats.implicits._

type ErrorOr[A] = Either[Vector[String], A]

val e1: ErrorOr[Int] = Left(Vector("error 1"))
val e2: ErrorOr[Int] = Left(Vector("error 2"))

// sequentil semantic
(e1, e2).tupled

// parallel semantic
(e1, e2).parTupled

val ok1: ErrorOr[Int] = Right(4)
val ok2: ErrorOr[Int] = Right(5)

val f: (Int, Int) => Int = (x, y) => x * y

// collect all errors if exists
(e1, e2) parMapN f
(e1, ok2) parMapN f

(ok1, ok2) parMapN f
