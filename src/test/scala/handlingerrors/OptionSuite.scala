package handlingerrors

import org.scalatest.FunSuite
import Option._

class OptionSuite extends FunSuite {

  trait TestOption {
    val opt1 = Some(5)
    val opt2 = Some(opt1)
    val opt3 = Some(10)
    val xs = Seq(1.0, 2.0, 3.0, 4.0)
  }

  new TestOption {

    test("map") {
      assert(opt1.map(_ + 5) == Some(10))
    }

    test("flatMap") {
      assert(opt2.flatMap(o => o.map(_ + 10)) == Some(15))
    }

    test("getOrElse") {
      assert(None.getOrElse(opt1) == Some(5))
      assert(opt2.getOrElse(opt3) == opt1)
    }

    test("orElse") {
      assert(None.orElse(opt1) == Some(5))
      assert(None.orElse(None).orElse(opt1) == Some(5))
    }

    test("mean") {
      assert(mean(xs) == Some(2.5))
    }

    test("variance") {
      assert(variance(xs) == Some(1.25))
    }

    test("map2") {
      assert(map2(opt1, opt3)(_ + _) == Some(15))
    }

    test("sequence") {
      assert(sequence(List(None)) == None)
      assert(sequence(List(opt1, opt3)) == Some(List(5, 10)))
      assert(sequence(List(opt1, opt3, None)) == None)
    }

    test("traverse") {
      val ys = List(1, 2, 3, 4)
      assert(traverse(ys)(y => if (y < 4) Some(y) else None) == None)
      assert(
        traverse(ys)(y => if (y < 5) Some(y) else None) == Some(
          List(1, 2, 3, 4)))
    }

  }

}
