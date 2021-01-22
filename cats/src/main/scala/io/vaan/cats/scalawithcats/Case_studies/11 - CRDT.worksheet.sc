import cats.kernel.CommutativeMonoid
import cats.implicits._

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intBoundedSemiLattice = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int): Int = a1 max a2
    def empty: Int = 0
  }

  implicit def setBoundedSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
    def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
    def empty: Set[A] = Set.empty[A]
  }
}

final case class GCounter[A](counters: Map[String, A])(implicit m: BoundedSemiLattice[A]) {
  def increment(host: String, amount: A): GCounter[A] = {
    val value = amount |+| counters.getOrElse(host, m.empty)
    GCounter(counters + (host -> value))
  }

  def merge(that: GCounter[A]): GCounter[A] = 
    GCounter(this.counters |+| that.counters)
   
  def total: A = 
    m.combineAll(counters.values)
}

// gcounter
val v0i = GCounter(Map("A" -> 0, "B" -> 0, "C" -> 0))
val v1i = v0i.increment("A", 3)
val v2i = v1i.increment("B", 4)

val v3i = v2i.merge(GCounter(Map("A" -> 7, "B" -> 2, "C" -> 0)))

v3i.total

// gset
val v0s = GCounter(Map("A" -> Set.empty[Int], "B" -> Set.empty[Int], "C" -> Set.empty[Int]))
val v1s = v0s.increment("A", Set(1, 2))
val v2s = v1s.increment("B", Set(3, 4))

val v3s = v2s.merge(GCounter(Map("A" -> Set(2, 3), "B" -> Set(3), "C" -> Set.empty[Int])))

v3s.total
