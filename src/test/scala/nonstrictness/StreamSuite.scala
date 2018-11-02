package nonstrictness

import org.scalatest.FunSuite
import Stream._

class StreamSuite extends FunSuite {

  trait TestStream {
    val xs = Stream(1, 2, 3, 4)
  }

  new TestStream {

    test("headOption") {
      assert(xs.headOption == Some(1))
    }

    test("toList") {
      assert(xs.toList == List(1, 2, 3, 4))
    }

    test("toList2") {
      assert(xs.toList == List(1, 2, 3, 4))
    }

    test("take") {
      assert(xs.take(1).toList2 == List(1))
      assert(xs.take(3).toList2 == List(1, 2, 3))
      assert(xs.take(5).toList2 == List(1, 2, 3, 4))
    }

    test("drop") {
      assert(xs.drop(1).toList2 == List(2, 3, 4))
      assert(xs.drop(3).toList2 == List(4))
      assert(xs.drop(6).toList2 == Nil)
    }

    test("takeWhile") {
      assert(xs.takeWhile(_ < 3).toList2 == List(1, 2))
      assert(xs.takeWhile(_ > 5).toList2 == Nil)
    }

    test("exists") {
      assert(xs.exists(_ == 1) == true)
      assert(xs.exists(_ == 5) == false)
    }

    test("exists2") {
      assert(xs.exists2(_ == 1) == true)
      assert(xs.exists2(_ == 5) == false)
    }

    test("forall") {
      assert(xs.forall(_ < 5) == true)
      assert(xs.forall(_ < 4) == false)
    }

    test("takeWhile2") {
      assert(xs.takeWhile2(_ < 3).toList2 == List(1, 2))
      assert(xs.takeWhile2(_ > 2).toList2 == Nil)
    }

    test("headOption2") {
      assert(xs.headOption2 == Some(1))
    }

    test("map") {
      assert(xs.map(_ + 1).toList == List(2, 3, 4, 5))
    }

    test("filter") {
      assert(xs.filter(_ < 4).toList == List(1, 2, 3))
    }

    test("append") {
      assert(xs.append(Stream(5, 6)).toList == List(1, 2, 3, 4, 5, 6))
    }

    test("flatMap") {
      assert(
        xs.flatMap(x => Stream(x, x)).toList == List(1, 1, 2, 2, 3, 3, 4, 4))
    }

    test("find") {
      assert(xs.find(_ == 2) == Some(2))
    }

    test("ones") {
      assert(ones.exists(_ == 1))
    }

    test("constant") {
      val c = constant(1)
      assert(c.exists(_ % 2 != 0))
      assert(c.map(_ + 1).exists(_ % 2 == 0))
    }

    test("from") {
      val f = from(1).take(5).toList
      assert(f == List(1, 2, 3, 4, 5))
    }

    test("fibs") {
      assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    }

    test("ones2") {
      assert(ones2.exists(_ == 1))
    }

    test("constant2") {
      val c = constant2(1)
      assert(c.exists(_ % 2 != 0))
      assert(c.map(_ + 1).exists(_ % 2 == 0))
    }

    test("from2") {
      val f = from2(1).take(5).toList
      assert(f == List(1, 2, 3, 4, 5))
    }

    test("map2") {
      assert(xs.map2(_ + 1).toList == List(2, 3, 4, 5))
    }

    test("take2") {
      assert(xs.take2(1).toList2 == List(1))
      assert(xs.take2(3).toList2 == List(1, 2, 3))
      assert(xs.take2(5).toList2 == List(1, 2, 3, 4))
    }

    test("zipWith") {
      assert(xs.zipWith(xs)(_ + _).toList == List(2, 4, 6, 8))
      assert(xs.zipWith(Stream(1,2))(_ + _).toList == List(2,4))
      assert(Stream(1,2,3).zipWith(xs)(_ + _).toList == List(2,4,6))
    }

    test("zipAll") {
      assert(xs.zipAll(xs).toList == List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), Some(3)),
        (Some(4), Some(4))
      ))
      assert(xs.zipAll(Stream(1,2)).toList == List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), None),
        (Some(4), None)
      ))
      assert(Stream(1,2).zipAll(xs).toList == List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (None, Some(3)),
        (None, Some(4))
      ))
    }

    test("startsWith") {
      assert(xs.startsWith(xs))
      assert(xs.startsWith(Stream(1)))
      assert(!xs.startsWith(Stream(2)))
    }

    test("tails") {
      assert(xs.tails.map(_.toList).toList == List(
        List(1, 2, 3, 4),
        List(2, 3, 4),
        List(3, 4),
        List(4),
        List())
      )
    }

    test("hasSubsequence") {
      assert(xs.hasSubsequence(Stream(2,3)))
    }

    test("tails via scanRight") {
      assert(xs.scanRight(Stream[Int]())((a,b) => cons(a, b)).map(_.toList).toList == List(
        List(1, 2, 3, 4),
        List(2, 3, 4),
        List(3, 4),
        List(4),
        List())
      )
    }

  }

}
