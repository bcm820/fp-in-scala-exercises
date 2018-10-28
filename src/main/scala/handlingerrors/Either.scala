package handlingerrors

// hide std library `Option` and `Either`
import scala.{Option => _, Either => _, Left => _, Right => _, _}

// Either represents values that can be one of two things (a disjoint union of two types).
// When used to indicate success/failure, Right is reserved for success, and Left for failure.
// Sometimes Either is used more generally to encode one of two possibilities
sealed trait Either[+E, +A] {

  // 4.6
  // Implement map, flatMap, orElse, and filter that operate on the Right value.

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }

  // When mapping over the right side, we must promote the left type parameter
  // to some supertype, to satisfy the +E variance annotation.
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e)  => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case Left(_)  => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for (x <- this; y <- b) yield f(x, y)

}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  // Rather than return `None` if xs.isEmpty, return an explanation
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  // Of course, it's better to include more error info (i.e. a stack trace)
  // Try implements the common pattern of converting thrown exceptions to values
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // 4.7
  // Implement sequence and traverse for Either.
  // These should return the first error that’s encountered, if there is one.

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil    => Right(Nil)
    case h :: t => h.map2(sequence(t))(_ :: _)
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil    => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

}
