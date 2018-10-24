package gettingstarted

object Generics extends App {

  // Polymorphic/generic function
  // Returns first index in array that passes a test, or -1 if not found
  def findFirst[T](as: Array[T], p: T => Boolean): Int = {
    def loop(n: Int): Int =
      if (n == as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }
  val ss = Array("hello", "world")
  println("ss contains w: " + findFirst(ss, (s: String) => s.contains("w")))

  /*
   * 2.2
   * isSorted checks whether an Array[A] is sorted
   * according to a given comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    loop(0)
  }
  val ns = Array(1, 2, 3, 6, 5)
  println("ns is sorted: " + isSorted(ns, (x: Int, y: Int) => x < y))

  /*
   * Polymorphic functions limit implementations
   * e.g. This partial application must only be implemented in one way.
   * Since partial1 must receive a B and return a C, it must return a call of `f`.
   * Why? Because no other C is available to return except via calling `f`.
   */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  /*
   * 2.3
   * Currying converts a function of two args into a partially applied fn with one arg.
   * Here there is only one implementation possible.
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  /*
   * 2.4
   * Reverses the transformation of curry.
   * Note: Std lib has `Function.uncurried` that can be used for uncurrying
   */
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)

  /*
   * 2.5
   * HOF that composes two functions
   * Here g(a) returns a B, which f receives and returns a C.
   * This is so common in Scala that we can simply say `f compose g` or `f andThen g`.
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
