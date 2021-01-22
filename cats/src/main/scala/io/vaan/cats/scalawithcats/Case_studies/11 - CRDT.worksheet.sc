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

// GCounter type class
trait GCounter[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)
        (implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])
        (implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])
        (implicit m: CommutativeMonoid[V]): V
}

// KVStore type class
trait KeyValueStore[F[_,_]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

// KVStore instances
object KeyValueStore {
  implicit def mapKVStore[K, V] = new KeyValueStore[Map] {
    override def put[K, V](map: Map[K,V])(k: K, v: V): Map[K,V] = 
      map + (k -> v)
    
    override def get[K, V](map: Map[K,V])(k: K): Option[V] = 
      map.get(k)

    override def getOrElse[K, V](map: Map[K,V])(k: K, default: V): V = 
      map.getOrElse(k, default)
    
    override def values[K, V](map: Map[K,V]): List[V] = 
      map.values.toList
  }
}

// syntax for KVStore
implicit class KvsOps[F[_,_], K, V](f: F[K, V]) {
  def put(key: K, value: V)
        (implicit kvs: KeyValueStore[F]): F[K, V] =
    kvs.put(f)(key, value)

  def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
    kvs.get(f)(key)

  def getOrElse(key: K, default: V)
        (implicit kvs: KeyValueStore[F]): V =
    kvs.getOrElse(f)(key, default)

  def values(implicit kvs: KeyValueStore[F]): List[V] =
    kvs.values(f)
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

// GCounter with KVStore
implicit def gcounterInstance[F[_,_], K, V]
    (implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]) =
  new GCounter[F, K, V] {
    def increment(f: F[K, V])(key: K, value: V)
          (implicit m: CommutativeMonoid[V]): F[K, V] = {
      val total = f.getOrElse(key, m.empty) |+| value
      f.put(key, total)
    }

    def merge(f1: F[K, V], f2: F[K, V])
          (implicit b: BoundedSemiLattice[V]): F[K, V] =
      f1 |+| f2

    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.combineAll
  }

val g1 = Map("a" -> 7, "b" -> 3)
val g2 = Map("a" -> 2, "b" -> 5)

val counter = GCounter[Map, String, Int]

val merged = counter.merge(g1, g2)
val total  = counter.total(merged)
