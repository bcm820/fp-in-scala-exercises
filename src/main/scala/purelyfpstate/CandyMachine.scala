package purelyfpstate

/* 6.11
Implement a finite state automaton that models a simple candy dispenser.
The machine has two types of input:
- Inserting a coin
- Turning a knob to dispense candy
It can be in one of two states: locked or unlocked.
It tracks how many candies are left and how many coins it contains.

The rules of the machine are as follows:
-Inserting a coin into a locked machine will unlock it if there’s candy.
-Turning the knob on an unlocked machine will dispense candy and lock it.
-Inserting a coin into an unlocked machine does nothing.
-Turning the knob on a locked machine does nothing.
-A machine that’s out of candy ignores all inputs.

The method `simulateMachine` operates the machine based on a list of inputs
and returns the number of coins and candies left in the machine at the end.
e.g. 10 coins and 5 candies -> 4 candies bought -> output: (14, 1). */

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  import State._

  def update(i: Input): State[Machine, Unit] =
    modify(m =>
      (i, m) match {
        case (Coin, Machine(true, candy, coin)) if candy > 0 =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) if candy > 0 =>
          Machine(true, candy - 1, coin)
        case _ => m
    })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(update))
      m <- get
    } yield (m.candies, m.coins)
}
