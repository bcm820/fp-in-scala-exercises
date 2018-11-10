package purelyfpstate

import org.scalatest.FunSuite
import State._

class StateSuite extends FunSuite {

  trait TestState {
    type Rand[A] = State[RNG, A]
    def int: Rand[Int] = State(_.nextInt)
    def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
  }

  new TestState {

    test("unit returns state with the value provided") {
      assert(unit(5).run(1) == (5, 1))
    }

    test("map returns unchanged state with a mapped value") {
      assert(unit(5).map(_ + 5).run(1) == (10, 1))
    }

    test("flatMap and map allow for-comprehensions") {
      val ns: Rand[(Int, Int)] = for {
        x <- int
        y <- int
      } yield (x, y)
      val (p, _) = ns.run(RIG(1))
      assert(p._1 != p._2)
    }

  }

}
