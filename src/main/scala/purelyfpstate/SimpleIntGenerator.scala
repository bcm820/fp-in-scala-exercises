package purelyfpstate

// Stateful referential transparency
// Rather than update the state as a side effect,
// return the new state along with its computed value in a tuple.
// Caller then uses the computed next state throughout the program.
trait RNG {
  def nextInt: (Int, RNG)
}

// This is a "random" simple integer generator that purely
// generates the same sequence of values given the same n.
// Uses a seed value, bitwise AND, right binary shift with zero fill.
case class RIG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextGen = RIG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextGen)
  }
}
