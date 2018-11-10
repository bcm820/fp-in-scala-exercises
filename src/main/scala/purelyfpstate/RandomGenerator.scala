package purelyfpstate

object RandomGenerator {

  // 6.1
  // Write a function that uses RNG.nextInt to generate
  // a random integer between 0 and Int.MaxValue (inclusive).
  // Handle the corner case when nextInt returns Int.MinValue,
  // which doesn’t have a non-negative counterpart.
  def nonNegInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (math.abs(i), r)
  }

  // 6.2
  // Write `double` to generate a double between 0 and 1 (non-incl).
  // Note: Use Int.MaxValue to obtain the maximum positive integer value,
  // and you can use x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3
  // Write functions to generate an (Int, Double) pair,
  // a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
  // using the functions you’ve already written.

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  // Write a function to generate a list of random integers
  // Here I use a corecursive fn for generating a list of ints.
  // Uses List factory method iterate with `f` called on for start value
  // e.g. List(start, f(start), f(f(start)), f(f(f(start)))
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val randList = List.iterate((1, rng), count + 1) {
      case (_, r) => nonNegInt(r)
    }
    (randList.map(_._1).tail, randList.last._2)
  }

  /* A better API for state actions
  Notice that each function above has the same type RNG => (A, RNG).
  Such functions are called state actions since they transform RNG states
  from one to the next. These actions can be combined using combinators.
  Rather than passing state along, we want our combinators to do that.
  Below we start using a Rand[+A] type alias as a defined state action
  that uses some RNG to generate an A, and transitions the RNG state. */
  type Rand[+A] = RNG => (A, RNG)

  // Generates an int given some RNG, using RNG.nextInt
  def int: Rand[Int] = _.nextInt

  // Simple RNG state transition which passes RNG state without using it
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // Map for transforming a state action's output without modifying the state
  // Note that `rng2` stays the same, and only `a` is modified by `f`.
  // This allows for a kind of function composition for a generated value.
  def map[A, B](action: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = action(rng)
    (f(a), rng2)
  }

  // An example of how map can be used. Receives some RNG to generate an int,
  // and then transforms the int into a value gte 0 and divisible by 2.
  def nonNegEven: Rand[Int] = map(int)(i => i - i % 2)

  // 6.5
  // Use map to reimplement double in a more elegant way.
  def double2: Rand[Double] = map(int)(_ / (Int.MaxValue.toDouble + 1))

  // 6.6
  // Write combinator map2 to combine two RNG actions using a binary function.
  // Takes two actions, a1 and a2, and a function f for combining their results,
  // and returns a new action that combines them.
  def map2[A, B, C](a1: Rand[A], a2: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = a1(rng)
      val (b, rng3) = a2(rng2)
      (f(a, b), rng3)
    }

  // An example of how map2 can be used. Receives some RNG and performs
  // two state actions, combining their result values into a pair
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  // We can use `both` to reimplement intDouble and doubleInt from 6.3
  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // 6.7
  // If you can combine two RNG transitions, you can combine a list of them.
  // Implement `sequence` for combining a list of transitions into one.
  // Since lists associate to the right, we use foldRight with Nil as base case
  def sequence[A](as: List[Rand[A]]): Rand[List[A]] =
    as.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  // Use `sequence` to reimplement `ints` (6.4).
  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  /* Nesting state actions
  map and map2 are powerful combinators, but some fns can't be written with them.
  For example, consider how we'd implement nonNegLessThan(n). If the int generated
  is greater than n, we need to retry. But how, if we can't reference the RNG?
  We need a more powerful combinator, flatMap. */

  // 6.8
  // Implement `flatMap` which uses Rand[A] to generate a Rand[B].
  // Here the transformed state is explicitly passed along to `f(a)`.
  def flatMap[A, B](action: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = action(rng)
    f(a)(rng2)
  }

  // Use `flatMap` to implement `nonNegLessThan`.
  def nonNegLessThan(n: Int): Rand[Int] =
    flatMap(nonNegInt)(i => {
      val mod = i % n
      if (mod < n) unit(mod)
      else nonNegLessThan(n)
    })

  // 6.9
  // Reimplement map and map2 in terms of flatMap.
  // Note: This is what makes flatMap more powerful than map/map2.
  def map3[A, B](action: Rand[A])(f: A => B): Rand[B] =
    flatMap(action)(a => unit(f(a)))

  def map4[A, B, C](a1: Rand[A], a2: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(a1)(a => map(a2)(b => f(a, b)))

}
