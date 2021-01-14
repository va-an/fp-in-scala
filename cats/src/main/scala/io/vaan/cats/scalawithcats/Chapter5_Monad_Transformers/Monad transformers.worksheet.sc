import cats.data.OptionT
import cats.implicits._

type ListOption[A] = OptionT[List, A]

// apply
val r1: ListOption[Int] = OptionT(List(Option(4)))

// pure
val r2: ListOption[Int] = OptionT.pure(4)
val r3: ListOption[Int] = 4.pure[ListOption]

for {
  x1 <- r1
  x2 <- r2
  x3 <- r3
} yield x1 + x2 + x3

// wrap Either around Option 
type ErrorOr[A] = Either[String, A]
type ErrorOrOption[A] = OptionT[ErrorOr, A]

val a = 14.pure[ErrorOrOption]
val b = 6.pure[ErrorOrOption]

a.flatMap(aa => b.map(bb => aa + bb))
for {aa <- a; bb <- b } yield aa + bb

// go deeper
import cats.data.{OptionT, EitherT}
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

val feo: FutureEitherOption[Int] = for {
  a <- 4.pure[FutureEitherOption]
  b <- 17.pure[FutureEitherOption]
} yield (a * b)

Await.result(feo.value.value, 1 second)

// exercise
type Response[A] = EitherT[Future, String, A]

val powerLevels = Map(
  "Jazz"      -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"   -> 10
)

def getPowerLevel(autobot: String): Response[Int] =
  EitherT.cond(
    powerLevels.contains(autobot),
    powerLevels(autobot),
    s"Autobot $autobot unreachable"
  )
  
getPowerLevel("Jazz")
getPowerLevel("Vaan")

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for {
    pl1 <- getPowerLevel(ally1)
    pl2 <- getPowerLevel(ally2)
  } yield (pl1 + pl2) > 15

canSpecialMove("Jazz", "Bumblebee")
canSpecialMove("Jazz", "Hot Rod")
canSpecialMove("Hot Rod", "Vaan")

def tacticalReport(ally1: String, ally2: String) = {
  Await.result(canSpecialMove(ally1, ally2).value, 1 second) match {
    case Left(msg)    => s"Comms error: $msg"
    case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) => s"$ally1 and $ally2 need a recharge."
  }
}

tacticalReport("Jazz", "Bumblebee")
tacticalReport("Jazz", "Hot Rod")
tacticalReport("Hot Rod", "Vaan")
