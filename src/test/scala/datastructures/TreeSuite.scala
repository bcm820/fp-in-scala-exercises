package datastructures.tree

import org.scalatest.FunSuite
import Tree._

class TreeSuite extends FunSuite {

  trait TestTree {
    val t =
      Branch(
        Branch(Leaf(23), Branch(Leaf(40), Leaf(13))),
        Branch(Branch(Leaf(41), Leaf(39)), Leaf(12))
      )
    val t2 = Branch(
      Branch(Leaf(46), Branch(Leaf(80), Leaf(26))),
      Branch(Branch(Leaf(82), Leaf(78)), Leaf(24))
    )
  }

  new TestTree {

    test("size") {
      assert(size(t) == 11)
    }

    test("maximum") {
      assert(maximum(t) == 41)
    }

    test("depth") {
      assert(depth(t) == 3)
    }

    test("map") {
      assert(map(t)(_ * 2) == t2)
    }

    test("size2") {
      assert(size2(t) == 11)
    }

    test("maximum2") {
      assert(maximum2(t) == 41)
    }

    test("depth2") {
      assert(depth2(t) == 3)
    }

    test("map2") {
      assert(map2(t)(_ * 2) == t2)
    }

    test("fold") {
      assert(fold(t)(a => a)(_ + _) == 168)
    }

  }

}
