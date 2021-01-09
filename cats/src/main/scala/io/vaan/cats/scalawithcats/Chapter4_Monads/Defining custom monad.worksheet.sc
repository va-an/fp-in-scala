import cats.Monad
import cats.implicits._
import scala.annotation.tailrec

// custom option monad
val optionMonad = new Monad[Option] {
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = 
    fa flatMap f
  
  @tailrec
  def tailRecM[A, B](a: A)(f: A => Option[Either[A,B]]): Option[B] = 
    f(a) match {
      case None           => None
      case Some(Left(a1)) => tailRecM(a1)(f) 
      case Some(Right(b)) => b.some
    }
  
  def pure[A](x: A): Option[A] = 
    x.some
}

optionMonad.pure[Int](4)
optionMonad.flatMap(4.some)(x => (x * x).some)

// retry flatMap is not stack-safe
def retry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
  f(start).flatMap { a =>
    retry(a)(f)
  }

val f:Int => Option[Int] = 
  a => 
    if (a == 0) None
    else Some(a - 1)

retry(100)(f)

// retry(10000)(f) // StackOverflowError

// stack-safe retry with tailRecM
def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] = 
  Monad[F].tailRecM(start) { a => 
    f(a).map(a2 => Left(a2))
  }

retryTailRecM(10000)(f) // works fine

// iterateWhileM
def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
  start.iterateWhileM(f)(a => true)

retryM(10000)(f)

// exercise
sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

implicit val treeMonad = new Monad[Tree] {
  def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = 
    tree match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f)) 
      case Leaf(value) => f(value)
    }

  def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = 
    flatMap(f(a)) {
      case Left(value) => tailRecM(value)(f)
      case Right(value) => Leaf(value)
    }
  
  def pure[A](x: A): Tree[A] =
    Leaf(x)
}


branch(
  left = leaf(100), 
  right = leaf(200)
)
  .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

for {
  a <- branch(leaf(100), leaf(200))
  b <- branch(leaf(a - 10), leaf(a + 10))
  c <- branch(leaf(b - 1), leaf(b + 1))
} yield c
