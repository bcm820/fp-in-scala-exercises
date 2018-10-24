package datastructures

object TreeOps extends App {
  import Tree._

  val t =
    Branch(
      Branch(Leaf(23), Branch(Leaf(40), Leaf(13))),
      Branch(Branch(Leaf(41), Leaf(39)), Leaf(12))
    )

  size(t) // 11
  maximum(t) // 41
  depth(t) // 3
  map(t)(_ * 2)
  // Branch(
  //   Branch(Leaf(46),Branch(Leaf(80),Leaf(26))),
  //   Branch(Branch(Leaf(82),Leaf(78)),Leaf(24))
  // )

  size2(t) // 11
  maximum2(t) // 41
  depth2(t) // 3
  map2(t)(_ * 2)
  // Branch(
  //   Branch(Leaf(46),Branch(Leaf(80),Leaf(26))),
  //   Branch(Branch(Leaf(82),Leaf(78)),Leaf(24))
  // )

  fold(t)(a => a)(_ + _) // sum: 168

}
