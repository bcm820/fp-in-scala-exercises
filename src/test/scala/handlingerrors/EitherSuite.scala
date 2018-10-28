package handlingerrors

import org.scalatest.FunSuite
import Either._

class EitherSuite extends FunSuite {

  trait TestEither {
    val e1 = Right(5)
    val e2 = Right(10)
    val e3 = Try("20".toInt)
    val e4 = Try("".toInt)
  }

  new TestEither {

    test("map") {
      assert(e1.map(_ + 5) == Right(10))
    }

    test("flatMap") {
      assert(e1.flatMap(o => e2.map(_ + o)) == Right(15))
      assert((for (a <- e1; b <- e2) yield a + b) == Right(15))
      assert((for (a <- e1; b <- e2; c <- e3) yield a + b + c) == Right(35))
      assert(
        (for (a <- e1; b <- e2; c <- e4)
          yield a + b + c).isInstanceOf[Left[Exception]]
      )
    }

    test("orElse") {
      assert(e4.orElse(e1) == Right(5))
      assert(e4.orElse(e4).orElse(e1) == Right(5))
    }

    test("map2") {
      assert(e1.map2(e2)(_ + _) == Right(15))
      assert(e1.map2(e2)(_ + _).map2(e3)(_ + _) == Right(35))
    }

    test("sequence") {
      assert(sequence(List(Left(""))) == Left(""))
      assert(sequence(List(e1, e2)) == Right(List(5, 10)))
      assert(sequence(List(e1, e2, e3)) == Right(List(5, 10, 20)))
      assert(sequence(List(e1, e2, e3, Left(""))) == Left(""))
    }

    test("traverse") {
      val xs = List(1, 2, 3, 4)
      assert(traverse(xs)(x => if (x < 4) Right(x) else Left("")) == Left(""))
      assert(
        traverse(xs)(x => if (x < 5) Right(x) else Left("")) == Right(
          List(1, 2, 3, 4)))
    }

  }

}
