import cats.{Monoid, Applicative, Id}
import cats.implicits._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

// single-threaded MapReduce method
def foldMap[A, B: Monoid]
  (values: Vector[A])
  (f: A => B): B = 
  // xs.map(f).fold(Monoid[B].empty)((a, b) => a |+| b)
  // xs.foldLeft(Monoid[B].empty)((a, b) => a |+| f(b))
  values.foldLeft(Monoid[B].empty)(_ |+| f(_))

val xs = Vector(1, 2, 3)

foldMap(xs)(x => x * x)
foldMap(xs)(x => s"$x! ")
foldMap("so wut".toVector)(_.toString.toUpperCase)

// multi-threaded
def parFoldMap[A, B: Monoid]
  (values: Vector[A])
  (f: A => B): Future[B] = {
    val numCores  = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    
    values
      .grouped(groupSize)
      .toVector
      .traverse(g => Future(foldMap(g)(f)))
      .map(_.combineAll)
  }

parFoldMap(xs)(x => x * x)
      