import cats.implicits._

3.asRight[Throwable]

// from option
Either.fromOption[String, Int](1.some, "boom")
Either.fromOption[String, Int](None, "boom")

Either.catchOnly[ArithmeticException](4 / 0)

// ensure
-4.asRight[String].ensure("sub zero")(_ > 0)

// recover functions
"error".asLeft[Int].recover {
    case _ => -1
}

"error".asLeft[Int].recoverWith {
    case _ => Right(-1)
}

// left biased map
"error".asLeft[Int].leftMap(_.reverse)

// bimap
4.asRight[String].bimap(x => s"from bimap - $x", x => x * x)
"error".asLeft[Int].bimap(x => s"from bimap - $x", x => x * x)

// swap - exchange left and right
4.asRight[String].swap
"error".asLeft[Int].swap
