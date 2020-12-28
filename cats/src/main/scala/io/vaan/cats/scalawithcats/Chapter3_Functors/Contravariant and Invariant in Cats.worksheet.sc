import cats.{Contravariant, Show, Monoid}
import cats.implicits._

// contravarian examples
val showString = Show[String]

val showSymbol = Contravariant[Show]
    .contramap(showString)((sym: Symbol) => s"${sym.name}")

showSymbol.show(Symbol("vaan"))

showString
    .contramap[Symbol](sym => s"${sym.name}")
    .show(Symbol("vaan"))

// invariant examples
implicit val symbolMonoid: Monoid[Symbol] = 
    Monoid[String].imap(Symbol.apply)(_.name)

Monoid[Symbol].empty
Symbol("hello ") |+| Symbol("nice ") |+| Symbol("cats")
