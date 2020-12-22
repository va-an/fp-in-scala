// typeclass itself
trait Num[A] {
  def plus(x: A, y: A): A
  def minus(x: A, y: A): A
}

object Num {
  implicit val IntNum: Num[Int] = new Num[Int] {
    override def plus(x: Int, y: Int): Int = x + y
    override def minus(x: Int, y: Int): Int = x - y
  }

  implicit val DoubleNum: Num[Double] = new Num[Double] {
    override def plus(x: Double, y: Double): Double = x + y
    override def minus(x: Double, y: Double): Double = x - y
  }
}

// interface syntax
object NumSyntax {
  implicit class NumOps[A](x: A) {
    def plus(y: A)(implicit num: Num[A]): A = num.plus(x, y)
    def minus(y: A)(implicit num: Num[A]): A = num.minus(x, y)
  }
}

import NumSyntax._

1 plus 2
3.5 minus 0.4
