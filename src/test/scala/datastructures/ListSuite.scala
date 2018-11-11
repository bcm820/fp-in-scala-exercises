package datastructures.list

import org.scalatest.FunSuite
import List._

class ListSuite extends FunSuite {

  test("product") {
    val xs = List(1.0, 2.0, 3.0, 4.0)
    assert(product(xs) == 24.0)
  }

  test("sum") {
    val xs = List(1, 2, 3, 4)
    assert(sum(xs) == 10)
  }

  test("tail") {
    val xs = List(1, 2, 3, 4)
    assert(tail(xs) == List(2, 3, 4))
  }

  test("setHead") {
    val xs = List(1, 2, 3, 4)
    assert(setHead(xs, 0) == List(0, 2, 3, 4))
  }

  test("drop") {
    val xs = List(1, 2, 3, 4)
    assert(drop(xs, 2) == List(3, 4))
  }

  test("append") {
    val xs = List(1, 2, 3, 4)
    assert(append(xs, xs) == List(1, 2, 3, 4, 1, 2, 3, 4))
  }

  test("init") {
    val xs = List(1, 2, 3, 4)
    assert(init(xs) == List(1, 2, 3))
  }

  test("dropWhile & dropWhileInf") {
    val xs = List(1, 2, 3, 4)
    assert(dropWhile(xs, (x: Int) => x < 4) == List(4))
    assert(dropWhileInf(xs)(_ < 4) == List(4))
  }

  test("foldRight") {
    val xs = List(1, 2, 3, 4)
    val ys = List(1.0, 2.0, 3.0, 4.0)
    assert(sum2(xs) == 10)
    assert(product2(ys) == 24.0)
    assert(length(xs) == 4)
  }

  test("foldLeft") {
    val xs = List(1, 2, 3, 4)
    val ys = List(1.0, 2.0, 3.0, 4.0)
    assert(sum3(xs) == 10)
    assert(product3(ys) == 24.0)
    assert(length2(xs) == 4)
  }

  test("append2") {
    val xs = List(1, 2, 3, 4)
    assert(append2(xs, xs) == List(1, 2, 3, 4, 1, 2, 3, 4))
  }

  test("concat") {
    val xs = List(1, 2, 3, 4)
    assert(concat(List(xs, xs)) == List(1, 2, 3, 4, 1, 2, 3, 4))
  }

  test("add1") {
    val xs = List(1, 2, 3, 4)
    assert(add1(xs) == List(2, 3, 4, 5))
  }

  test("stringify") {
    val xs = List(1.0, 2.0, 3.0, 4.0)
    assert(stringify(xs) == List("1.0", "2.0", "3.0", "4.0"))
  }

  test("map") {
    val xs = List(1, 2, 3, 4)
    val ys = List(1.0, 2.0, 3.0, 4.0)
    assert(map(xs)(_ + 1) == List(2, 3, 4, 5))
    assert(map(ys)(_.toString) == List("1.0", "2.0", "3.0", "4.0"))
  }

  test("filter") {
    val xs = List(1, 2, 3, 4)
    assert(filter(xs)(_ < 3) == List(1, 2))
  }

  test("flatMap") {
    val xs = List(1, 2, 3, 4)
    assert(flatMap(xs)(x => List(x, x)) == List(1, 1, 2, 2, 3, 3, 4, 4))
  }

  test("filterWithFlatMap") {
    val xs = List(1, 2, 3, 4)
    assert(filterWithFlatMap(xs)(_ % 2 == 0) == List(2, 4))
  }

  test("addPos") {
    assert(addPos(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  test("zipWith") {
    assert(
      zipWith(List("A", "B", "C"), List("D", "E", "F"))(_ + _) == List("AD",
                                                                       "BE",
                                                                       "CF"))
  }

}
