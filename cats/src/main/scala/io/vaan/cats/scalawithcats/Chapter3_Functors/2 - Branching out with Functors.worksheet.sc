import cats.Functor
import cats.implicits._

sealed trait Tree[+A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

implicit val treeFunctor = new Functor[Tree] {
  override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = 
    tree match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
}

Tree.branch(Tree.leaf(10), Tree.leaf(20))
  .map(_ * 2)
