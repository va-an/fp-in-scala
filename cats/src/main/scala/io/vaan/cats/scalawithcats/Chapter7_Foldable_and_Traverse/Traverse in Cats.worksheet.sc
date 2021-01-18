import cats.Traverse
import cats.implicits._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

val xs = List(1, 2, 3)
val xsf = xs.map(x => Future.successful(x))
val xso = xs.map(_.some)

// List[Future[A]]
Traverse[List].sequence(xsf)
Traverse[List].traverse(xs)(x => Future.successful(x * x))

// syntax
xsf.sequence
xs.traverse(x => Future.successful(x * x))

// List[Option[A]]
Traverse[List].sequence(xso)
Traverse[List].traverse(xs)(x => (x * x).some)

// syntax
xso.sequence
xs.traverse(x => (x * x).some)
