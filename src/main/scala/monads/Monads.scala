package monads

/* Monad
A monad impleements one of the minimal sets of monadic combinators,
unit and flatMap OR unit and compose OR unit, map, and join,
satisfying the laws of associativity and identity.
It also implements map and map2, which is why it is also a functor.
*/
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // 11.3: Monadic Combinators
  // Many functions implemented for monad primitives can be
  // implemented once for all in this monad trait...

  // `sequence` combines a list of a monad type
  // into a single monad containing a list of its values.
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  // `traverse` maps over a list and applies some f
  // to return a monad containing a list of its results
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  // `replicateM` generates a List of n size of the monad
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  // `product` creates pairs from the values contained in each monad
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // `filterM`
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((x,y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x),y)(_ :: _) else y)(x))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

}

object Monads {

  val option = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  option.sequence(List(Some(1), None)) // None
  option.sequence(List(Some(1), Some(2))) // Some(List(1, 2))
  option.traverse(List(1, 2, 3))(a => Some(a + a)) // Some(List(2, 4, 6))
  option.replicateM(5, Some(1)) // Some(List(1, 1, 1, 1, 1))
  option.product(Some(1), Some(2)) // Some((1,2))
  option.product(Some(1), None) // None
  option.filterM(List(1,2,3,4))(a => Some(a % 2 == 0)) // Some(List(2, 4))

  val list = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  list.sequence(List(List(1), List(2), List(3))) // List(List(1, 2, 3))
  list.traverse(List(1, 2, 3))(a => List(a + a)) // List(List(2, 4, 6))
  list.replicateM(5, List(1)) // List(List(1, 1, 1, 1, 1))
  list.product(List(1, 2), List(3, 4)) // List((1,3), (1,4), (2,3), (2,4))
  list.filterM(List(1,2,3,4))(a => List(a % 2 == 0)) // List(List(2, 4))

}
