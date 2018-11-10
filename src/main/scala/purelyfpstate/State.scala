package purelyfpstate

/* Generalizing state actions
The combinators unit, map, flatMap, etc. are general-purpose functions
that can work for any state actions (not just Rand[A]). So we can then
define a new type alias (or case class!) used for handling types of state.
State is short for `computation that carries some state action along`. */

// 6.10
// Generalize unit, map, map2, flatMap, and sequence.

case class State[S, +A](run: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => { val (a, s1) = run(s); f(a).run(s1) })

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] = {
    def loop(s: S, as: List[State[S, A]], acc: List[A]): (List[A], S) =
      as match {
        case Nil    => (acc.reverse, s)
        case h :: t => val (a, s2) = h.run(s); loop(s2, t, a :: acc)
      }
    State(state => loop(state, actions, List()))
  }

  // To aid our imperative approach, we can add two primitive State combinators
  // for reading the state and for writing the state (i.e. get and set).
  // We can then use `modify` to update the state during a sequence of ops
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get // Gets the current state
      _ <- set(f(s)) // Sets the new state via f
    } yield ()

}

object StateExample {
  import State._

  type Rand[A] = State[RNG, A]
  val int: Rand[Int] = State(_.nextInt)
  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Since map and flatMap are defined, we can use for-comprehensions
  // for imperatively styled readability (while still maintaining purity)
  // Remember: Rand[A] expects a RNG, which is then passed to each map
  // Called via `ns.run(RIG(#))`
  val ns: Rand[List[Int]] = for {
    x <- int // Generates integer x
    y <- int // Generates nextInt y
    zs <- ints(5) // Generates list of 5 nextInts
  } yield zs.map(_ % (x + y)) // Performs computation

}
