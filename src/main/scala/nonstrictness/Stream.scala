package nonstrictness

// A stream is a lazy list, implemented as Empty and Cons
// Cons has a head and tail, both non-strict, declared as thunks
sealed trait Stream[+A] {
  import Stream._

  // note `h()` -- `h` only evaluated on demand
  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty      => None
  }

  // 5.1
  // Write a function to convert a Stream to a List, forcing its evaluation
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty      => Nil
  }

  // The above is not a tailrec solution; an accumulator should be used
  def toList2: List[A] = {
    def loop(xs: Stream[A], acc: List[A]): List[A] = xs match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case Empty      => acc
    }
    loop(this, Nil).reverse
  }

  // 5.2
  // Write the function `take` for returning the first `n` elements of a Stream.

  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t)           => cons(h(), t().take(n - 1))
    case Empty                => empty
  }

  // Write the function `drop` for skipping the first `n` elements of a Stream.
  // recursively returns tail until n == 0 or empty, then returns stream
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  // 5.3
  // Write the function takeWhile for returning all starting elements
  // of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  // note `t().exists(p)` only evaluated if `!p(h())`
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  // `=> B` might not be evaluated by `f` (recursion might not occur)
  // `f` can choose not to evaluate it and terminate the traversal early
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty      => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  // `b` will only be evaluated if `!p(a)`,
  // evaluates to p(a1) || p(a2) || p(a3)... || false
  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // 5.4
  // Implement forall (all elements in the Stream match a given predicate)
  // evaluates to p(a1) && p(a2) && p(a3)... && true
  def forall(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // 5.5
  // Use foldRight to implement takeWhile.
  // evaluates to cons(p(a1), cons(p(a2), cons(p(a3)... Nil)))
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  // 5.6
  // Implement `headOption` using foldRight.
  def headOption2: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7
  // Implement map, filter, append, and flatMap using foldRight.
  // The append method should be non-strict in its argument.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  // B>:A type parameter indicates that B must be equal to or a supertype of A.
  def append[B >: A](xs: => Stream[B]): Stream[B] =
    foldRight(xs)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  // `find` in terms of `filter` is easy since filter is iterated lazily
  // Rather than iterate all the way, it returns headOption once found
  def find(p: A => Boolean): Option[A] = filter(p).headOption


  // 5.13 (see companion object)
  // Write map, take, takeWhile, and zipWith (ch. 3) in terms of unfold.

  // Note shorter syntax in place of `stream => stream match { Cons(h,t) => ... }`
  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h,t) => Some((f(h()), t()))
  }

  // Tuple2 used in place of tuple syntax for readability
  def take2(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h,t), 1) => Some((h(), (empty, 0)))
    case (Cons(h,t), n) if n > 1 => Some(h(), Tuple2(t(), n - 1))
    case _ => None
  }

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), Tuple2(t1(), t2()))
    case _ => None
  }

  // Write `zipAll` in terms of unfold.
  // `zipAll` should continue the traversal as long as either stream has more elements
  // using Option to indicate whether each stream has been exhausted.
  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s)) {
    case (Cons(h,t), Empty) => Some(Tuple2(Some(h()), None), Tuple2(t(), empty))
    case (Empty, Cons(h,t)) => Some(Tuple2(None, Some(h())), Tuple2(empty, t()))
    case (Cons(h1,t1), Cons(h2,t2)) => Some(Tuple2(Some(h1()), Some(h2())), Tuple2(t1(), t2()))
    case _ => None
  }

  // 5.14
  // Implement `startsWith` to check if one stream is a prefix of another.
  // e.g. Stream(1,2,3) startsWith Stream(1,2)
  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty).forall(p => p._1 == p._2)

  // 5.15
  // Implement tails using unfold.
  // tails returns the stream of suffixes of the input sequence, starting with the original Stream.
  // e.g. Stream(1,2) tails returns Stream(Stream(1,2), Stream(2), Stream())
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some(s, s.drop(1))
  } append Stream(empty)

  // We can now implement `hasSubsequence` using the two functions above:
  def hasSubsequence[B](s: Stream[B]): Boolean = tails.exists(_.startsWith(s))

  // 5.16
  // Generalize tails to the function scanRight, which is like
  // a foldRight that returns a stream of the intermediate results.
  // e.g. Stream(1,2,3).scanRight(0)(_ + _).toList === List(1+2+3+0, 2+3+0, 3+0, 0)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((h, p) => {
      val b2 = f(h, p._1)
      (b2, cons(b2, p._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // note `apply` uses smart constructors defined below
  // args passed to `cons` are passed in as unevaluated thunks
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // smart constructors used for creating streams
  // head and tail cached as lazy values to avoid repeated evals
  def empty[A]: Stream[A] = Empty
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  // 5.8
  // Write `constant` which returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // 5.9
  // Write `from` that generates an infinite stream of integers
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  // Write `fibs` that generates Fibonacci numbers
  val fibs: Stream[Int] = {
    def loop(f0: Int, f1: Int): Stream[Int] = cons(f0, loop(f1, f0 + f1))
    loop(0, 1)
  }

  // 5.11
  // Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing
  // both the next value and the next state in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,t)) => cons(h, unfold(t)(f))
    case None => empty
  }

  /* The unfold function is an example of a `corecursive` function.
  Recursive functions consume data and terminate by recursing on smaller inputs,
  while corecursive functions produce data and need not terminate so long as they
  remain productive (we can evaluate more of the result by running `f`). */

  // 5.12
  // Write fibs, from, constant, and ones in terms of unfold.

  def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1)) // or use constant

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def from2(n: Int): Stream[Int] = unfold(n)(next => Some(next, next + 1))

  // Note shorter syntax in place of `pair => pair match { case (f0,f1) => ... }`
  val fibs2: Stream[Int] = unfold((0,1)) {
    case (a,b) => Some((a, Tuple2(b, a + b)))
  }

}
