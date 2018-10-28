package handlingerrors

// hide std library `Option`, `Some` and `Either`
import scala.{Option => _, Some => _, Either => _, _}

// Option represents that a function call's evaluation might be a value or not.
sealed trait Option[+A] {

  // 4.1
  // Implement map, flatMap, getOrElse, orElse, and filter.
  // Note use of lazy eval in getOrElse and orElse

  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None        => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None        => default
  }

  // orElse allows for short-circuiting, and can be chained
  // if Some, wrap in another Some; else map to None
  // `get` call either returns wrapped Some or Option B
  // Can be implemented via map(Some(_)).getOrElse(ob)
  def orElse[B >: A](optB: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None    => optB
  }

  // flatMap lets us construct a computation with multiple stages,
  // any of which may return None, causing the computation to abort
  // without executing the rest, since the resulting `None.flatmap(f)`
  // will immediately return None without running `f`.
  // Can be implemented via map(f).getOrElse(None)
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None    => None
  }

  // filter is used to convert Some options into Nones
  // Can be implemented via flatMap(a => if (f(a)) Some(a) else None)
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case None            => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  // Exceptions break referential transparency.
  // RT expressions may be subbed with the values they refer to..
  // But if we substitute throw new Exception("fail!") for y in x + y,
  // it produces a different result inside a try block. The fn returns 43.
  def fail(i: Int): Int = {
    // val y: Int = throw new Ecxeption("fail!") // error is thrown!
    // try { val x = 42; x + y }
    try { val x = 42; x + ((throw new Exception("fail!")): Int) } catch {
      case e: Exception => 43
    } // 43 is returned...
  }

  // Rather than throw an error if xs.isEmpty, we can return an Option.
  // Return type will consistently be either Some or None.
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  // Variance is the mean of `math.pow(x - m, 2)` for each element in a sequence.Z
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3
  // Write `map2` which combines two Option values using a binary function.
  // If either Option value is None, then the return value is too.
  // Via flatMap: a.flatMap(x => b.map(y => f(x, y)))
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for (x <- a; y <- b) yield f(x, y)

  // 4.4
  // Write `sequence` which combines a list of Options into one Option
  // containing a list of all the Some values in the original list.
  // If the list contains a None, the whole result should be None.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  // 4.5
  // Write `traverse`, which maps over a list using a function that might fail,
  // returning None if applying it to any element of the list returns None.
  // Try not to use `map` and `sequence`, visiting each element only once.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil    => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

}
