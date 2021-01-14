// never store money values in Double in production!

case class BTC(total: Double)

trait CurrencyOps[A] {
  def plus(x: A, y: A): A
}

object CurrencyOps {
  implicit val plusBTC = new CurrencyOps[BTC] {
    override def plus(x: BTC, y: BTC): BTC = BTC(x.total + y.total)
  }

  def plus[A](a: A, b: A)(implicit ops: CurrencyOps[A]) = ops.plus(a, b)
}

object CurrencySyntax {
  implicit class Ops[A, B](x: A) {
    def +(y: A)(implicit ops: CurrencyOps[A]): A = ops.plus(x, y)
  }
}

import CurrencyOps._
import CurrencySyntax._

plus(BTC(0.005), BTC(2.5))
BTC(0.005) + BTC(2.5)

// TODO: how to add different currencies?
// case class LTC(total: Double)
// BTC(0.005) + LTC(2.5)