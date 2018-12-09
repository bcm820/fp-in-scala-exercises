package applicatives

import monads.Functor

/* Applicative Functor is a variation on Monad.
Its primitive combinators are unit and map2. */
trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(Unit))((a, _) => f(a))

  // many other combinators that use map2 and unit can be derived...
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

}

object Applicatives {

  val optionAp = new Applicative[Option] {
    def unit[A](a: => A) = Some(a)
    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      (fa, fb) match { // explicitly written to avoid use of flatMap
        case (Some(a), Some(b)) => Some(f(a, b))
        case _                  => None
      }
  }

  optionAp.sequence(List(Some(1), Some(2), Some(3))) // Some(List(1, 2, 3))
  optionAp.map3(Some(1), Some(2), Some(3))(_ + _ + _) // Some(6)

  val streamAp = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a.zip(b).map(f.tupled)
  }

  streamAp.map2(Stream(1, 2), Stream(3, 4))(_ + _) // Stream(4, ?)
  streamAp.map2(Stream(1, 2), Stream(3, 4))(_ + _).toList // List(4, 6))

}
