import cats.Show
import cats.syntax.show._
import io.vaan.datatype.Cat

// we have implicits for library types
val showInt: Show[Int] = Show.apply[Int]
val showString: Show[String] = Show[String] // apply

showInt.show(12)
showString.show("hello")

// we have not implicit for this type
//val showCat: Show[Cat] = Show.apply[Cat]

// but if we create implicit instance for Show[Cat], this is will works
implicit val catShow: Show[Cat] =
  (cat: Cat) => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."

// now works
// search from implicits is ok now
val showCat: Show[Cat] = Show.apply[Cat]

// show me your Barsik
val barsik = Cat("Barsik", 4, "white")
barsik.show

// we cat create instances easier
val showCat2 = Show.show[Cat] { cat =>
  val name = cat.name.show
  val age = cat.age.show
  val color = cat.color.show

  s"$name is a $age year-old $color cat."
}
