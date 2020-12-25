trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

object MonoidLaws {
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }
}

object booleanMonoids {
  implicit val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
    override def empty: Boolean = false
  }

  implicit val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
    override def empty: Boolean = true
  }
}

import MonoidLaws._
import booleanMonoids._

// TODO: how to not pass implicit value into laws-methods?

// andMonoid
andMonoid.combine(true, true)
andMonoid.combine(true, false)

associativeLaw(true, true, false)(andMonoid)
identityLaw(true)(andMonoid)

// orMonoid
orMonoid.combine(true, true)
orMonoid.combine(true, false)

associativeLaw(true, true, false)(orMonoid)
identityLaw(true)(orMonoid)
