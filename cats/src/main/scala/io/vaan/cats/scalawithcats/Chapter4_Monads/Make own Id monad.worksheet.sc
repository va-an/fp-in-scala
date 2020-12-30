import cats.Id

object IdMonad {
    def pure[A](value: A): Id[A] =
        value
    
    def flatMap[A, B](m: Id[A])(f: A => Id[B]): Id[B] = 
        f(m)
    
    def map[A, B](m: Id[A])(f: A => B): Id[B] =
        flatMap(m)(x => pure(f(x))) // actually we can use f(m) here
}

val value = IdMonad.pure(4)
IdMonad.flatMap(value)(x => x * x)
IdMonad.map(4)(x => x * x)
