package nonstrictness

object StreamExample {

  // thunks are unevaluated forms of expressions
  // the expressions are only evaluated by reference
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  // `lazy` delays evaluation of a val until first reference
  // the val is then cached and no longer evaluated on reference
  lazy val lv = if2(false, "TRUE!", "FALSE!")
  lv // "False!"

  // All of our Stream function implementations are incremental.
  // They don't generate their results until forced into a List.
  // The intermediate results of each function are not yet evaluated.
  // Example: Program trace for Stream (all evaluate to List(12, 14))
  import Stream._
  Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  Stream(11, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  Stream(2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  Stream(12, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  12 :: Stream(3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  12 :: Stream(13, 4).map(_ + 10).filter(_ % 2 == 0).toList
  12 :: Stream(4).map(_ + 10).filter(_ % 2 == 0).toList
  12 :: Stream(14).map(_ + 10).filter(_ % 2 == 0).toList
  12 :: 14 :: empty[Int].map(_ + 10).filter(_ % 2 == 0).toList
  List(12, 14)

  // Note we don’t fully instantiate the intermediate stream that results from the map.
  // It’s exactly as if we had interleaved the logic using a special- purpose loop.
  // For this reason, people sometimes describe streams as “first-class loops”
  // whose logic can be combined using higher-order functions like map and filter.
  // Since intermediate streams aren’t instantiated, it’s easy to reuse existing
  // combinators in novel ways without having to worry that we’re doing more processing
  // of the stream than necessary.

}
