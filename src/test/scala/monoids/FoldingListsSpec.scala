package monoids

import FoldingLists._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object FoldingListsSpec extends Properties("Folds") {

  property("seqIntIsOrdered") = forAll { (xs: List[Int]) =>
    if (xs == xs.sorted) seqIntIsOrdered(xs)
    else !seqIntIsOrdered(xs)
  }

}
