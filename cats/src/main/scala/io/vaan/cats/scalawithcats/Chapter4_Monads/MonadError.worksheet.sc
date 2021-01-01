import scala.util.Try
import cats.MonadError
import cats.implicits._

type ErrorOr[A] = Either[String, A]
val monadError = MonadError[ErrorOr, String]

monadError.pure(4)
4.pure[ErrorOr]

val failure = monadError.raiseError[String]("boom")

monadError.handleErrorWith(failure) {
    case "boom" => monadError.pure("gotcha")
    case _ => monadError.raiseError("unexpected")
}

monadError.handleError(failure) {
    case "boom" => "gotcha"
    case _ => "unexpected"
}

monadError.ensure(4.pure[ErrorOr])("too low")(_ > 10)
4.pure[ErrorOr].ensure("too low again")(_ > 10)

// exercise 
def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.ensure(age.pure[F])(
        new IllegalArgumentException("Age must be greater than or equal to 18")
    )(_ >= 18)

validateAdult[Try](18)
validateAdult[Try](8)

type ExceptionOr[A] = Either[Throwable, A]
validateAdult[ExceptionOr](4)
