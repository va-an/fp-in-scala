import io.vaan.datatype.Cat
import cats.data.Writer
import cats.implicits._

val log = Vector(
    "one cat",
    "two cat"
)

val barsik = Cat("barsik", 4, "black")

// log and result
// apply
val w = Writer(
    log,
    barsik
)

// syntax
barsik.writer(log)

// create result with empty log
type Logged[A] = Writer[Vector[String], A]
barsik.pure[Logged]

// create log without result
log.tell

// get log and result
w.written
w.value
val (logs, result) = w.run

(for {
    a <- 4.pure[Logged]
    _ <- Vector("a", "b").tell
    b <- 8.writer(Vector("c", "d"))
} yield a + b).run

// map logs
w.mapWritten(_.map(_.toUpperCase()))
    .run

// map log and res
w.bimap(
    log => log.map(_.toUpperCase),
    cat => cat.copy(name = cat.name.toUpperCase())
).run

w.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val cat2 = res.copy(name = res.name + "!")

    (log2, cat2)
}.run

// clear the log
w.reset.run

w.swap.run

// exercise
def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

def factorial(n: Int): Logged[Int] = 
    for {
        ans <- {
            if (n == 0) 1.pure[Logged]
            else slowly(factorial(n - 1).map(_ * n))
        }

        _ <- Vector(s"fact $n $ans").tell
    } yield ans

factorial(5).run

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

Await.result(Future.sequence(Vector(
  Future(factorial(5)),
  Future(factorial(5))
)).map(_.map(_.written)), 5.seconds)

