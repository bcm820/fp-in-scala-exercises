package monoids

import org.scalatest.FunSuite
import WordCount._

class WordCountSuite extends FunSuite {

  test("length 3: 'a b c'") {
    assert(count("a b c") == 3)
  }

  test("length 5: 'lorem ipsum dolor sit amet, '") {
    assert(count("lorem ipsum dolor sit amet, ") == 5)
  }

}
