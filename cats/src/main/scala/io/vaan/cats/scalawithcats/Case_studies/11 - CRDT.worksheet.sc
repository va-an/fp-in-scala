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

trait GCounter[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)
        (implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])
        (implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])
        (implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_,_], K, V]
        (implicit counter: GCounter[F, K, V]) =
    counter

  implicit def mapGCounter[K, V] = new GCounter[Map, K, V] {
    def increment(map: Map[K,V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K,V] = {
      val value = v |+| map.getOrElse(k, m.empty)
      map + (k -> v)
    }
  
    def merge(map1: Map[K,V], map2: Map[K,V])(implicit b: BoundedSemiLattice[V]): Map[K,V] = 
      map1 |+| map2
    
    def total(map: Map[K,V])(implicit m: CommutativeMonoid[V]): V = 
      m.combineAll(map.values)
  }
}

val g1 = Map("a" -> 7, "b" -> 3)
val g2 = Map("a" -> 2, "b" -> 5)

val counter = GCounter[Map, String, Int]

val merged = counter.merge(g1, g2)
val total  = counter.total(merged)
