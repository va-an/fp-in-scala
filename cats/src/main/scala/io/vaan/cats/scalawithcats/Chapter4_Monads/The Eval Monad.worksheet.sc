// intro to eval
val f: String => Double = 
    functionName => {
        println(s"computing $functionName")
        math.random
    }

// call-by-value
// eager memoized computation
val x = f("x")
x
x

// call-by-name
// lazy not memoized computation
def y = f("y")

y
y

// call-by-needk
// lazy memoized computation
lazy val z = f("z")

z
z

// Eval in Cats
import cats.Eval

// call-by-value
val now = Eval.now(f("now"))

// call-by-name
val always = Eval.always(f("always"))

// call-by-need
val later = Eval.later(f("later"))

now.value
now.value

always.value
always.value

later.value
later.value

val greeting = Eval
    .always{
        println("side effect 1")
        "hello, "
    }.map { x =>
        println("side effect 2")
        x + "cats!"
    }

greeting.value

// mapping always works as "def" semantics
val evalChain = for {
    a <- Eval.now {
        println("calc A")
        4
    }

    // always run even if Eval.now
    b <- Eval.always {
        println("calc B")
        8
    }
} yield {
    println("calc A + B")
    a + b
}

evalChain.value
evalChain.value

val chainMemoize = evalChain.memoize

// side effect here
chainMemoize.value

// no effects
chainMemoize.value
