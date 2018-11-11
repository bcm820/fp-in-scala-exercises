package monoids

import Monoids._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object MonoidsSpec extends Properties("Monoids") {

  property("stringMonoid associativity") = forAll {
    (a: String, b: String, c: String) =>
      {
        val op = stringMonoid.op _
        op(op(a, b), c) == op(a, op(b, c))
      }
  }

  property("stringMonoid identity") = forAll { (a: String) =>
    {
      val op = stringMonoid.op _
      val zero = stringMonoid.zero
      op(a, zero) == a && op(zero, a) == a
    }
  }

  property("listMonoid associativity") = forAll {
    (a: List[Int], b: List[Int], c: List[Int]) =>
      {
        val lm = listMonoid[Int]
        val op = lm.op _
        op(op(a, b), c) == op(a, op(b, c))
      }
  }

  property("listMonoid identity") = forAll { (a: List[Int]) =>
    {
      val lm = listMonoid[Int]
      val op = lm.op _
      val zero = listMonoid.zero
      op(a, zero) == a && op(zero, a) == a
    }
  }

  property("intAddition associativity") = forAll { (a: Int, b: Int, c: Int) =>
    {
      val op = intAddition.op _
      op(op(a, b), c) == op(a, op(b, c))
    }
  }

  property("intAddition identity") = forAll { (a: Int) =>
    {
      val op = intAddition.op _
      val zero = intAddition.zero
      op(a, zero) == a && op(zero, a) == a
    }
  }

  property("intMultiplication associativity") = forAll {
    (a: Int, b: Int, c: Int) =>
      {
        val op = intMultiplication.op _
        op(op(a, b), c) == op(a, op(b, c))
      }
  }

  property("intMultiplication identity") = forAll { (a: Int) =>
    {
      val op = intMultiplication.op _
      val zero = intMultiplication.zero
      op(a, zero) == a && op(zero, a) == a
    }
  }

  property("booleanOr associativity") = forAll {
    (a: Boolean, b: Boolean, c: Boolean) =>
      {
        val op = booleanOr.op _
        op(op(a, b), c) == op(a, op(b, c))
      }
  }

  property("booleanOr identity") = forAll { (a: Boolean) =>
    {
      val op = booleanOr.op _
      val zero = booleanOr.zero
      op(a, zero) == a && op(zero, a) == a
    }
  }

  property("booleanAnd associativity") = forAll {
    (a: Boolean, b: Boolean, c: Boolean) =>
      {
        val op = booleanAnd.op _
        op(op(a, b), c) == op(a, op(b, c))
      }
  }

  property("booleanAnd identity") = forAll { (a: Boolean) =>
    {
      val op = booleanAnd.op _
      val zero = booleanAnd.zero
      op(a, zero) == a && op(zero, a) == a
    }
  }

  property("optionMonoid associativity") = forAll {
    (a: Option[Int], b: Option[Int], c: Option[Int]) =>
      {
        val om = optionMonoid[Int]
        val op = om.op _
        op(op(a, b), c) == op(a, op(b, c))
      }
  }

  property("optionMonoid identity") = forAll { (a: Option[Int]) =>
    {
      val om = optionMonoid[Int]
      val op = om.op _
      val zero = optionMonoid.zero
      op(a, zero) == a && op(zero, a) == a
    }
  }

  property("endoMonoid associativity") = forAll {
    (a: Boolean => Boolean, b: Boolean => Boolean, c: Boolean => Boolean) =>
      {
        val em = endoMonoid[Boolean]
        val op = em.op _
        val left = op(op(a, b), c)(true)
        val right = op(a, op(b, c))(true)
        left == right
      }
  }

  property("endoMonoid identity") = forAll { (a: Boolean => Boolean) =>
    {
      val em = endoMonoid[Boolean]
      val op = em.op _
      val zero: Boolean => Boolean = endoMonoid.zero
      val left = op(a, zero)(true) == a(true)
      val right = op(zero, a)(false) == a(false)
      left && right
    }
  }

}
