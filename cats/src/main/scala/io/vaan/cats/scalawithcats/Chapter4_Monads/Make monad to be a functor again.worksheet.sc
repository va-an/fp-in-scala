trait Monad[F[_]] {
    // pure and flatMap for monad
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

    // map for fun
    def map[A, B](value: F[A])(f: A => B): F[B] =
        flatMap(value)(x => pure(f(x)))
}

val optionMonad = new Monad[Option] {
    override def pure[A](value: A): Option[A] =
        Option(value)

    override def flatMap[A, B](value: Option[A])(f: A => Option[B]): Option[B] = 
        value flatMap f     
}

// shining as monad
optionMonad.pure(4)
optionMonad.flatMap(Option(4))(x => Option(x * x))

// free functor for our monad from Monad-trait
optionMonad.map(Option(4))(x => x * x)
