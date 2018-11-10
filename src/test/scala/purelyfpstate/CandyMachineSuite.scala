package purelyfpstate

import org.scalatest.FunSuite
import Machine._

class CandyMachineSuite extends FunSuite {

  trait TestMachine {
    val m1 = Machine(true, 5, 5)
    val m2 = Machine(false, 5, 5)
    val m3 = Machine(false, 0, 5)
  }

  new TestMachine {

    test("coin into locked machine unlocks and adds a coin") {
      assert(update(Coin).run(m1) == ((), Machine(false, 5, 6)))
    }

    test("turning unlocked machine locks and removes candy") {
      assert(update(Turn).run(m2) == ((), Machine(true, 4, 5)))
    }

    test("coin into unlocked machine does nothing") {
      assert(update(Coin).run(m2) == ((), m2))
    }

    test("turning locked machine does nothing") {
      assert(update(Turn).run(m1) == ((), m1))
    }

    test("coin into machine with no candy does nothing") {
      assert(update(Coin).run(m3) == ((), m3))
    }

    test("turning machine with no candy does nothing") {
      assert(update(Turn).run(m3) == ((), m3))
    }

    test("alternating inputs deplete candy, add coins") {
      val is = List(Coin, Turn, Coin, Turn, Coin, Turn)
      assert(simulateMachine(is).run(m1) == ((2, 8), Machine(true, 2, 8)))
    }

    test("only coin inputs adds just one coin, unlocks machine") {
      val is = List(Coin, Coin, Coin)
      assert(simulateMachine(is).run(m1) == ((5, 6), Machine(false, 5, 6)))
    }

    test("only turn inputs does nothing") {
      val is = List(Turn, Turn, Turn)
      assert(simulateMachine(is).run(m1) == ((5, 5), m1))
    }

    test("identical sequential inputs are ignored") {
      val is = List(Coin, Turn, Turn, Coin, Coin)
      assert(simulateMachine(is).run(m1) == ((4, 7), Machine(false, 4, 7)))
    }

  }

}
