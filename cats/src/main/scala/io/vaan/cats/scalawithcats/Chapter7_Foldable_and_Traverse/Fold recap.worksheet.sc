import cats.implicits._

def show[A](xs: List[A]): String =
  xs.foldLeft("nil")((acc, curr) => s"$curr then $acc")

show(Nil)
show(List(1, 2, 3))

val xs = List(1, 2, 3)

xs.foldLeft(List.empty[Int])((a, i) => i :: a)
xs.foldRight(List.empty[Int])(_ :: _)

object ListOps {
  def map[A, B](xs: List[A])(f: A => B): List[B] = 
    xs.foldRight(List.empty[B])((i, a) => f(i) :: a)

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    xs.foldRight(List.empty[B])((i, a) => f(i) ::: a)

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    xs.foldRight(List.empty[A])((i, a) =>
      if (f(i)) i :: a
      else a
    )
}

ListOps.map(xs)(x => x * x)
ListOps.flatMap(xs)(x => (x * x).pure[List])
ListOps.filter(xs)(x => x >= 2)

