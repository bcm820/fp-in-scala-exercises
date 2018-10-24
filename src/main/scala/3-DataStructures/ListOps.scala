package datastructures

object ListOps extends App {
  import List._

  val d = Nil
  val c = Cons(4.0, d)
  val b = Cons(3.0, c)
  val a = Cons(2.0, b)
  val xs = Cons(1.0, a) // List(1.0, 2.0, 3.0, 4.0)
  product(xs) // 24.0

  val ys = List(1, 2, 3, 4)
  sum(ys) // 10
  tail(ys) // List(2,3,4)
  setHead(ys, 0) // List(0,2,3,4)
  drop(ys, 2) // List(3,4)
  append(xs, ys) // List(1,2,3,4,1,2,3,4)
  init(ys) // List(1,2,3)

  dropWhile(ys, (y: Int) => y < 4) // List(4)
  dropWhileInf(ys)(_ < 4) // List(4)

  // foldRight
  sum2(ys) // 10
  product2(xs) // 24.0
  length(xs) // 4

  // foldLeft
  sum3(ys) // 10
  product3(xs) // 24.0
  length2(xs) // 4

  append2(List(1, 2), List(3, 4)) // List(1,2,3,4)
  concat(List(List(1, 2), List(3, 4))) // List(1,2,3,4)

  add1(ys) // List(2,3,4,5)
  stringify(xs) // List("1.0", "2.0", "3.0", "4.0")

  map(ys)(_ + 1) // List(2,3,4,5)
  map(xs)(_.toString) // List("1.0", "2.0", "3.0", "4.0")
  filter(ys)(_ < 3) // List(1,2)
  flatMap(ys)(y => List(y, y)) // List(1,1,2,2,3,3,4,4)
  filterWithFlatMap(ys)(_ % 2 == 0) // List(2,4)

  addPos(List(1, 2, 3), List(4, 5, 6))
  zipWith(List("A", "B", "C"), List("D", "E", "F"))(_ + _)

}
