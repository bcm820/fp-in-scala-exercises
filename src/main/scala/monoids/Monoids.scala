package monoids

/* An introduction to purely algebraic data structures
A monoid is a simple structure defined only by its algebra.
It is often all we need to write useful, polymorphic functions.

A monoid consists of the following laws:
1. Associativity. e.g. `op(op(x,y),z) == op(x,op(y,z))`
2. Idenity. e.g. `op(x, zero) == x` and `op(zero, x) == x` */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  val zero: A
}

object Monoids {

  def stringMonoid = new Monoid[String] {
    def op(a: String, b: String) = a + b
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a: List[A], b: List[A]) = a ++ b
    val zero = Nil
  }

  // 10.1
  // Give Monoid instances for integer addition
  // and multiplication as well as the Boolean operators.
  def intAddition = new Monoid[Int] {
    def op(a: Int, b: Int) = a + b
    val zero = 0
  }

  def intMultiplication = new Monoid[Int] {
    def op(a: Int, b: Int) = a * b
    val zero = 1
  }

  def booleanOr = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    val zero = false
  }

  def booleanAnd = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a && b
    val zero = true
  }

  // 10.2
  // Give a Monoid instance for combining Option values
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]) = a orElse b
    val zero = None
  }

  def optionMonoidOpp[A] = dual(optionMonoid[A])

  // Every monoid has a `dual` where the `op` combines in the opposite order.
  // We can get the dual of a monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(a: A, b: A): A = m.op(b, a)
    val zero = m.zero
  }

  // 10.3
  // A function having the same argument and return type is
  // called an endofunction. Write a monoid for endofunctions.
  // f(x) compose g(x) == f(g(x)). g(x) is evaluated first.
  // f(x) andThen g(x) == g(f(x)). f(x) is evaluated first.
  def endoMonoid[A] = new Monoid[A => A] {
    // def op(f: A => A, g: A => A) = f compose g
    def op(f: A => A, g: A => A) = f andThen g
    val zero = (a: A) => a
  }

  // 10.4: TODO (see test folder for ScalaCheck use)
  // Use the property-based testing framework we developed in part 2
  // to implement a property for the monoid laws.
  // Use your property to test the monoids we’ve written.

}
