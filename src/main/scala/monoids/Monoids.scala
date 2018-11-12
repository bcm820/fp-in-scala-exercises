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

  /* Composing monoids
  The real power of monoids comes from the fact that they compose.
  For monoids A and B, the tuple (A, B) is also a monoid (product!).
  This allows us to assemble and compose more complex monoids,
  which we can use to perform multiple calculations simultaneously
  when folding a data structure.
  */

  // 10.16
  // Write productMonoid.
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]) = new Monoid[(A,B)] {
      def op(x: (A,B), y: (A,B)) = (A.op(x._1, y._1), B.op(x._2, y._2))
      val zero = (A.zero, B.zero)
    }

  // Using productMonoid, we can take calculate the mean of a list of ints:
  def mean(xs: List[Int]) = {
    val m = productMonoid(intAddition, intAddition)
    val p = ListFolds.foldMap(xs, m)(i => (1, i))
    p._2 / p._1.toDouble
  }
  
  // Monoid for merging two maps if their value types are also monoids
  // Note the convention of capitalizing the monoid value being mapped
  def mapMergeMonoid[K, V](M: Monoid[V]) = new Monoid[Map[K, V]] {
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, M.op(a.getOrElse(k, M.zero), b.getOrElse(k, M.zero)))
      }
    val zero = Map[K, V]()
  }

  // Example of assembling more complex monoids from mapMergeMonoid
  // When merging two nested maps, corresponding int values will be added
  def mapIntMonoid[A]: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
  def nestedMapIntMonoid[A,B]: Monoid[Map[A, Map[B, Int]]] =
    mapMergeMonoid(mapIntMonoid)

  // 10.17
  // Write a monoid instance for functions whose result value types are monoids
  // Pass a monoid of the value type returned by each function
  def functionMonoid[A, B](B: Monoid[B]) = new Monoid[A => B] {
    def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
    val zero = a => B.zero
  }

  // 10.18
  // A bag is like a Set, but represented by a map with one entry per element,
  // the element being the key and the value being the element's count in the bag.
  // e.g. `bag(List(a, rose, is, a, rose)) == Map(a -> 2, rose -> 2, is -> 1)
  // Use monoids to compute a bag from an IndexedSeq.
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ListFolds.foldMapV(as, mapIntMonoid[A])(a => Map(a -> 1))

}
