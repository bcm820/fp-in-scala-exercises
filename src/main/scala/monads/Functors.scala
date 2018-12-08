package monads

/* Functor: Generalizing the map function
A functor is any data type that implements map, which as we
know lifts a function with one argument into a context.
We can express this as a trait defined as a higher-kinded type.
The F[_] is a type of Functor (e.g. List, Option) */

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // Using map, we can identify other algebraic laws...

  // In the case of F[(A,B)], we can "distribute"
  // the F over the pairs and return (F[A], F[B]).
  // e.g. List((1,2), (3,4), (5,6)) -> (List(1,3,5), List(2,4,6))
  // This generalization of `unzip` can be used on any functor!
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  // Since F[(A,B)] is a product, we can construct
  // the opposite operation over a sum/coproduct.
  // e.g. Left(List(1,2,3)) -> List(Left(1), Left(2), Left(3))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }

}

object Functors {

  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }

  val optionFunctor = new Functor[Option] {
    def map[A, B](o: Option[A])(f: A => B): Option[B] = o match {
      case Some(a) => Some(f(a))
      case None    => None
    }
  }

}
