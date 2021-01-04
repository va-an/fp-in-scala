import cats.data.State

val a = State[Int, String] { state =>
  (state, s"state: $state")
}

// get state and result
val (state, result) = a.run(4).value

// get only state
a.runS(4).value

// get only result
a.runA(4).value

// composing state
val step1 = State[Int, String] { x =>
  val result = x + x
  (result, s"step 1 result: $result")
}

val step2 = State[Int, String] { x =>
  val result = x * x
  (result, s"step 2 result: $result")
}

val steps: State[Int, String] = for {
  s1 <- step1
  s2 <- step2
} yield s"$s1; $s2"

steps.run(4).value

// the same
step1.flatMap(s1 => step2.map(s2 => s"$s1; $s2"))
  .run(4).value

// useful ops
val getDemo = State.get[Int]
getDemo.run(4).value

val setDemo = State.set(16)
setDemo.run(2).value

val pureDemo = State.pure[Int, String]("pure!")
pureDemo.run(0).value

val inspectDemo = State.inspect[Int, String](x => s"extracted from state: $x")
inspectDemo.run(4).value

val modifyDemo = State.modify[Int](x => x * x)
modifyDemo.run(4).value

val z = for {
  a <- State.get[Int]
  _ <- State.set(a * a)
  b <- State.get[Int]
  _ <- State.modify[Int](_ - a)
  c <- State.inspect[Int, Int](_ - 2)
  _ <- State.modify[Int](_ + 3)
} yield (a + b + c)

z.run(4).value

// exercise - post-order calculator
type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] = {
  if (sym.forall(_.isDigit)) {
    val digit = sym.toInt
    State.modify[List[Int]](stack => stack :+ digit).map(_ => digit) // FIXME: appending to end of list, meh
  } else {
      State[List[Int], Int] { stack => 
        val last = stack.last
        val lastLast = stack(stack.size - 2)
        
        val result = sym match {
          case "+" => lastLast + last
          case "-" => lastLast - last
          case "/" => lastLast / last
          case "*" => lastLast * last
        }

        val newStack = stack.slice(0, stack.size - 2) :+ result

        (newStack, result)
      }
    }
  } 

evalOne("4").run(List.empty).value
evalOne("+").run(List(1, 2, 3)).value
evalOne("-").run(List(1, 2, 3)).value
evalOne("/").run(List(1, 10, 3)).value
evalOne("*").run(List(1, 2, 3)).value

(for {
  _ <- evalOne("1")
  _ <- evalOne("2")
  answer <- evalOne("+")
} yield answer)
  .run(List.empty)
  .value

def evalAll(input: List[String]): CalcState[Int] = 
  input.foldLeft(State.pure[List[Int], Int](0)) { (state, sym) => 
    state.flatMap(_ => evalOne(sym))
  }

evalAll(List("1", "2", "+", "3", "*"))
  .run(List.empty)
  .value

(for {
  _ <- evalAll(List("1", "2", "+"))
  _ <- evalAll(List("3", "4", "+"))
  answer <- evalOne("*")
} yield answer)
  .runA(List.empty)
  .value

def evalInput(input: List[String]): Int =
  evalAll(input)
  .runA(List.empty)
  .value

evalInput(List("1", "2", "+", "3", "*"))
