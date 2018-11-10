package purelyfpstate

import org.scalatest.FunSuite
import RandomGenerator._

class RandomGeneratorSuite extends FunSuite {

  trait TestRNG {
    val s = RIG(42)
  }

  new TestRNG {

    test("nonNegInt returns a non-negative int") {
      assert(nonNegInt(s)._1 > 0)
    }

    test("double returns a random double between 0 and 1") {
      val d = double(s)._1
      assert(d > 0 && d < 1)
    }

    test("intDouble returns a pair of a random int and double") {
      val id = intDouble(s)._1
      assert(id._1.isInstanceOf[Int])
      assert(id._2.isInstanceOf[Double])
    }

    test("doubleInt returns a pair of a random double and int") {
      val di = doubleInt(s)._1
      assert(di._1.isInstanceOf[Double])
      assert(di._2.isInstanceOf[Int])
    }

    test("double3 returns a tuple of three random doubles") {
      val ddd = double3(s)._1
      assert(ddd._1.isInstanceOf[Double])
      assert(ddd._2.isInstanceOf[Double])
      assert(ddd._3.isInstanceOf[Double])
    }

    test("ints generates a list of random integers") {
      val xs = ints(5)(s)._1
      assert(xs.isInstanceOf[List[Int]])
      assert(xs.size == xs.toSet.size)
    }

    test("map transforms a state action's output without modifying state") {
      val x = map(unit(5))(_ + 5)(s)._1
      assert(x == 10)
    }

    test("nonNegEven maps a random int into a value gte 0 and divided by 2") {
      val x = nonNegEven(s)._1
      assert(x > 0 && x % 2 == 0)
    }

    test("double2 returns a random double between 0 and 1") {
      val d = double2(s)._1
      assert(d > 0 && d < 1)
    }

    test("map2 combines two RNG actions and applies f to their results") {
      val x = map2(unit(3), unit(5))(_ * _)(s)._1
      assert(x == 15)
    }

    test("sequence cmobines a list of RNG actions into one") {
      val xs = sequence(List(unit(3), unit(5), unit(2)))(s)._1
      assert(xs == List(3, 5, 2))
    }

    test("ints2 generates a list of random integers") {
      val xs = ints2(5)(s)._1
      assert(xs.isInstanceOf[List[Int]])
      assert(xs.size == xs.toSet.size)
    }

    test("nonNegLessThan generates a non-neg int less than given n") {
      val x = nonNegLessThan(5)(s)._1
      assert(x < 5 && x > -1)
    }

    test("map3 transforms a state action's output without modifying state") {
      val x = map3(unit(5))(_ + 5)(s)._1
      assert(x == 10)
    }

    test("map4 combines two RNG actions and applies f to their results") {
      val x = map4(unit(3), unit(5))(_ * _)(s)._1
      assert(x == 15)
    }

  }

}
