package monoids

import datastructures.tree._

/* Foldable data structures
We can write a common trait for foldable data structures
such as trees, lists, and streams.
Here we abstract over a type constructor `F`, as `F[_]`,
where the underscore indicates F is a type constructor
that takes one type argument (not a type).
This is a higher-order type constructor / higher-kinded type. */

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

object Foldables {

  // 10.12
  // Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].

  val foldableList = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  val foldableStream = new Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  val foldableIndexedSeq = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      ListFolds.foldMapV(as, mb)(f)
  }

  // 10.13
  // Implement a Foldable instance for the binary Tree data type from ch. 3.
  // Note: Relied on notebook again, as I did not know where to begin...
  val foldableTree = new Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a)      => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a)      => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }
  }

  // 10.14
  // Write a Foldable[Option] instance.
  val foldableOption = new Foldable[Option] {
    def foldRight[A, B](o: Option[A])(z: B)(f: (A, B) => B): B = o match {
      case Some(a) => f(a, z)
      case None    => z
    }
    def foldLeft[A, B](o: Option[A])(z: B)(f: (B, A) => B): B = o match {
      case Some(a) => f(z, a)
      case None    => z
    }
    override def foldMap[A, B](o: Option[A])(f: A => B)(mb: Monoid[B]): B =
      o match {
        case Some(a) => f(a)
        case None    => mb.zero
      }

  }

}