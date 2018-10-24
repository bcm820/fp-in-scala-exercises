package datastructures

// List is an example of an algebraic data type (ADT).
// An ADT is defined by one or more data constructors.
// The data type is the sum or union of its data constructors,
// and each data constructor is the product of its arguments.

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// companion object with functions for list ops
object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  // 3.2
  // tail removes the first element of a List
  def tail[A](as: List[A]): List[A] = as match {
    case Nil         => throw new Error("empty list")
    case Cons(_, bs) => bs
  }

  // 3.3
  // replace the head of a list
  def setHead[A](as: List[A], b: A): List[A] = as match {
    case Nil         => throw new Error("empty list")
    case Cons(_, bs) => Cons(b, bs)
  }

  // 3.4
  // remove the first n elements from a list
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil                     => throw new Error("empty list")
    case Cons(_, bs) if (n == 1) => bs
    case Cons(_, bs)             => drop(bs, n - 1)
  }

  // 3.5
  // keep removing elements while they match a predicate
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(b, bs) if f(b) => dropWhile(bs, f)
    case _                   => as
  }

  // Appending two lists
  // we only need to iterate through a1
  // since we simply replace a1's Nil with a2
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // 3.6
  // return a list without the last element
  // since this is an SLL, it is slow
  def init[A](as: List[A]): List[A] = as match {
    case Nil          => throw new Error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(b, bs)  => Cons(b, init(bs))
  }

  // Improving dropWhile HOF
  // If we separate the args into two lists (i.e. curry)
  // f's type will be inferred from as
  def dropWhileInf[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(b, bs) if f(b) => dropWhileInf(bs)(f)
    case _                   => as
  }

  // foldRight allows us to generalize sum and product to receive a HOF
  // example: how sum2(List(1,2,3)) evaluates using foldRight
  // foldRight(List(1, 2, 3), 0)(_ + _)
  // 1 + foldRight(List(2, 3), 0)(_ + _)
  // 1 + 2 + foldRight(List(3), 0)(_ + _)
  // 1 + 2 + 3 + foldRight(Nil, 0)(_ + _)
  // 1 + 2 + 3 + 0
  def foldRight[A,B](as: List[A], acc: B)(f: (A,B) => B): B = as match {
    case Nil         => acc
    case Cons(b, bs) => f(b, foldRight(bs, acc)(f))
  }
  def sum2(xs: List[Int]) = foldRight(xs, 0)(_ + _)
  def product2(xs: List[Double]) = foldRight(xs, 1.0)(_ * _)

  // 3.9
  // length of list using foldRight
  def length[A](xs: List[A]) = foldRight(xs, 0)((_, acc) => acc + 1)

  // 3.10
  // since foldRight is not tailrec, implement tailrec foldLeft
  def foldLeft[A,B](as: List[A], acc: B)(f: (B,A) => B): B = as match {
    case Nil         => acc
    case Cons(b, bs) => foldLeft(bs, f(acc, b))(f)
  }

  // 3.11
  // sum, product, and length in terms of foldLeft
  def sum3(xs: List[Int]) = foldLeft(xs, 0)(_ + _)
  def product3(xs: List[Double]) = foldLeft(xs, 1.0)(_ * _)
  def length2[A](xs: List[A]) = foldLeft(xs, 0)((acc, _) => acc + 1)

  // 3.12
  // reverse a list using a fold
  def reverse[A](as: List[A]) =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  // 3.13
  // foldRight in terms of foldLeft (can't do foldLeft via foldRight)
  def foldRight2[A,B](as: List[A], acc: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), acc)((b, a) => f(a, b))

  // 3.14
  // append in terms of fold
  // replaces the `Nil` constructor of the first list with the second
  def append2[A](a1: List[A], a2: List[A]) = foldRight2(a1, a2)(Cons(_, _))

  // 3.15
  // concatenate a list of lists into a single list (e.g. flatten)
  // its runtime should be linear, using functions already defined
  def concat[A](as: List[List[A]]): List[A] = foldLeft(as, List[A]())(append2)

  // 3.16
  // transform a list of integers by adding 1 to each element
  // foldRight works best since the acc
  def add1(xs: List[Int]) =
    foldRight2(xs, List[Int]())((h, acc) => Cons(h + 1, acc))

  // 3.17
  // transform a list of doubles into a list of their string representations
  def stringify(xs: List[Double]) =
    foldRight2(xs, List[String]())((h, acc) => Cons(h.toString, acc))

  // 3.18
  // write map to generalize transforming a list's elements
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, List[B]())((h, acc) => Cons(f(h), acc))

  // 3.19
  // filter
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)

  // 3.20
  // flatMap
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  // 3.21
  // filter in terms of flatMap
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  // 3.22
  // accept two lists of the same length and add elements in the same position
  def addPos(xs1: List[Int], xs2: List[Int]): List[Int] = (xs1, xs2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPos(t1, t2))
  }

  // 3.23
  // generalize the previous so it's not specific to integers or addition
  def zipWith[A,B,C](xs1: List[A], xs2: List[B])(f: (A,B) => C): List[C] = (xs1, xs2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // 3.24
  // implement hasSubsequence to check whether a List contains another list
  // note- wasn't able to get this one without looking up a solution
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }

}
