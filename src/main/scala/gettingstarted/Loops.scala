package gettingstarted

object Loops {

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  // 2.1
  // Write a function to compute the nth fibonacci number
  // Init acc to store the next value in the sequence,
  // which is the sum of the previous two values.
  // Countdown from `n` and return `prev` at base case.
  def fib(n: Int): Int = {
    def loop(n: Int, acc: Int, next: Int): Int =
      if (n == 0) acc
      else loop(n - 1, next, acc + next)
    loop(n, 0, 1)
    // loop(n - 1, 1, 0 + 1)
    // loop(n - 2, 1, 1 + 1)
    // loop(n - 3, 2, 1 + 2)
    // loop(n - 4, 3, 2 + 3)
    // loop(n - 5, 5, 3 + 5)
  }

}
