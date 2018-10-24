package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  // count the number of nodes in a tree
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  // return the maximum value in a Tree[Int]
  // use x.max(y) to compute the max of two integers
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // 3.27
  // return the maximum depth of the tree
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // 3.28
  // transform each value (map)
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  // generalize all the above via fold
  // Note: each data constructor receives a handler
  // f: what to do if leaf
  // g: what to do if branch
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => {
      val partialFold = fold(_: Tree[A])(f)(g)
      g(partialFold(l), partialFold(r))
    }
  }
  def size2[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)
  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depth2[A](t: Tree[A]): Int = fold(t)(a => 0)((l, r) => 1 + (l max r))
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
