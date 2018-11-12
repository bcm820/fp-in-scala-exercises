package monoids

import Monoids._

object ListFolds extends App {

  /* Folding lists with monoids
  Consider the signatures of foldLeft and foldRight on list:
    def foldRight[B](z: B)(f: (A, B) => B): B
    def foldLeft[B](z: B)(f: (B, A) => B): B
  If A and B are the same type, the monoid laws hold! */
  val xs = List(1, 2, 3, 4, 5, 6)
  val a = xs.foldRight(intAddition.zero)(intAddition.op)
  val b = xs.foldLeft(intAddition.zero)(intAddition.op)
  a == b // true!

  // A left fold associates ops to the left, with zero on the left.
  // A right fold associates ops to the right, with zero on the right.
  // We can write a general function that folds a list with a monoid:
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  // 10.5
  // What if our list has an element type without a Monoid instance?
  // We can always map over the list to turn it into a type that does...
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    // as.foldRight(m.zero)((a, b) => m.op(b, f(a)))
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // 10.6
  // Write foldLeft and foldRight using foldMap.
  // Note: Taken straight from the workbook -- no way I would've gotten these.

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  /* Associativity and parallelism
  We can reduce monoids sequentially using foldLeft or foldRight,
  but we can also use a balanced fold, which can be more efficient.
  Suppose we have a sequence `a, b, c, d` to reduce...
  - Right fold: `op(a, op(b, op(c, d)))`
  - Left fold: `op(op(op(a, b), c), d)`
  - Balanced fold: `op(op(a, b), op(c, d))`
  A balanced fold allows for parallelism since inner op calls are independent.
  Also, the balanced tree structure is efficient in cases where the cost of each
  op is proportional to the size of its arguments. */

  // 10.7
  // Implement a foldMap for IndexedSeq.
  // Your implementation should use a divide-and-conquer strategy,
  // and then add the answers together with the monoid.
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 0) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // 10.8: TODO
  // Also implement a parallel version of foldMap using the library from ch. 7.
  // Hint: Implement par, a combinator to promote Monoid[A] to a Monoid [Par[A]],
  // and then use this to implement parFoldMap.
  // import fpinscala.parallelism.Nonblocking._
  // def par[A](m: Monoid[A]): Monoid[Par[A]]
  // def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B]

  // 10.9
  // Use foldMapV to detect whether a given IndexedSeq[Int] is ordered.
  // Note: I used foldMap on List[Int] for a less complex challenge
  // since foldMapV doesn't evaluate in sequential order...
  // But this also assumes foldMap is defined via foldLeft,
  // since I am using Int.MinValue as the start value for m.zero
  def seqIntIsOrdered(ints: List[Int]): Boolean = {
    if (ints.toSet.size == 1) true
    else {
      def monoid = new Monoid[(Int, Boolean)] {
        def op(a: (Int, Boolean), b: (Int, Boolean)) =
          if (!a._2) a
          else if (a._1 > b._1) (a._1, false)
          else b
        val zero = (Int.MinValue, true)
      }
      foldMap(ints.map(i => (i, true)), monoid)(identity)._2
    }
  }

}