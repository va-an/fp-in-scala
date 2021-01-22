final case class GCounter(counters: Map[String, Int]) {
  def increment(host: String, amount: Int): GCounter = {
    val value = counters.getOrElse(host, 0) + amount
    GCounter(counters + ((host, value)))
  }

  def merge(that: GCounter): GCounter = {
    val newCounters = this.counters.foldLeft(Map.empty[String, Int]) { (a, i) => 
      i match {
        case (k, v) => a + (k -> v.max(that.counters.getOrElse(k, 0)))
      }
    }

    GCounter(newCounters)
  }
   
  def total: Int = 
    counters.values.sum
}

val v0 = GCounter(Map("A" -> 0, "B" -> 0, "C" -> 0))
val v1 = v0.increment("A", 3)
val v2 = v1.increment("B", 4)

val v3 = v2.merge(GCounter(Map("A" -> 7, "B" -> 2, "C" -> 0)))

v3.total
