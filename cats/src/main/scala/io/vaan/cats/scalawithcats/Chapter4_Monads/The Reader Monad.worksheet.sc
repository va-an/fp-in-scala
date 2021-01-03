import io.vaan.datatype.Cat
import cats.data.Reader

val catName: Reader[Cat, String] =
  Reader(cat => cat.name)

val barsik = Cat("barsik", 4, "red")

catName.run(barsik)
catName(barsik)

val helloCat: Reader[Cat, String] =
  catName.map(name => s"hello, $name!")

helloCat(barsik)

val showColor: Reader[Cat, String] =
  Reader(cat => s"${cat.color} colored cat")
  
val personalHello: Reader[Cat, String] = for {
  hello <- helloCat
  color <- showColor
} yield s"$hello $color"

personalHello(barsik)

// exercise
final case class Db(
  usernames: Map[Int, String],
  passwords: Map[String, String]
)

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] = 
    Reader(_.usernames.get(userId))

def checkPassword(
  username: String,
  password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)

val passwords = Map(
  "dade"  -> "zerocool",
  "kate"  -> "acidburn",
  "margo" -> "secret"
)

val db = Db(users, passwords)

findUsername(1)(db)
findUsername(777)(db)

checkPassword(username = "dade", password = "zerocool")(db)
checkPassword(username = "dade", password = "meow")(db)

def checkLogin(
  userId: Int,
  password: String): DbReader[Boolean] = for {
    maybeUser <- findUsername(userId)
    passwordFound <- maybeUser.map(username =>
      checkPassword(username, password)
    ).getOrElse(Reader[Db, Boolean](_ => false))
  } yield passwordFound
  
checkLogin(1, "zerocool")(db)
checkLogin(4, "davinci")(db)
