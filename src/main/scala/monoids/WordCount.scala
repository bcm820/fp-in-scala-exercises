package monoids

import ListFolds._

/* Parallel parsing
What if we wanted to determine the word count of a large document?
We'd need to split the document up as one string into manageable chunks
and then combine the intermediate results of the chunks associatively.
If a split occurs in the middle of a word, that could skew the count.
Here we create an ADT to handle partial results and track complete words.
Stub: Substring with no spaces found to the right AND left.
Part: Count of substrings with spaces, and stubs on right and left. */

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(left: String, words: Int, right: String) extends WC

object WordCount {

  // 10.10
  // Write a monoid instance for WC.
  def monoid = new Monoid[WC] {
    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(a), Stub(b))       => Stub(a + b)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(al, aw, ar), Part(bl, bw, br)) => {
        val arbl = if ((ar + bl).isEmpty) 0 else 1
        Part(al, aw + arbl + bw, br)
      }
    }
    val zero = Stub("")
  }

  // 10.11
  // Use the WC monoid to implement a function that
  // counts words in a String by recursively splitting it
  // into substrings and counting the words in those substrings.
  // Note: Had to rely on the notebook. This was tough!
  def count(s: String): Int = {

    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def charToMonoid(c: Char): WC =
      if (c == ' ') Part("", 0, "")
      else Stub(c.toString)

    def unstub(s: String): Int = if (s.isEmpty) 0 else 1

    foldMapV(s.toIndexedSeq, monoid)(charToMonoid) match {
      case Stub(s)       => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  /* Program trace:

  count("a b c")

  Convert to indexed sequence:
  Vector('a', ' ', 'b', ' ', 'c')

  Map each char to WC monoid:
  Vector(
    Stub(a), Part(, 0, ),
    Stub(b),
    Part(, 0, ), Stub(c)
  )

  foldMapV over the chunks using monoid.op:
  op(op(Stub(a), Part(, 0, )) op(Stub(b), op(Part(, 0, ), Stub(c))))
  op(Part(a, 0, )op(Stub(b), Part(, 0, c)))
  op(Part(a, 0, ) Part(b, 0, c))
  Part(a, 0 + 1 + 0, c)
  Part(a, 1, c)

  foldMapV call pattern match:
  unstub(a) + 1 + unstub(b)
  1 + 1 + 1
  3

 */

}
