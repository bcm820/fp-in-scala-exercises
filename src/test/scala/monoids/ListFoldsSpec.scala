package monoids

import ListFolds._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ListFoldsSpec extends Properties("Folds") {

  property("seqIntIsOrdered") = forAll { (xs: List[Int]) =>
    if (xs == xs.sorted) seqIntIsOrdered(xs)
    else !seqIntIsOrdered(xs)
  }

}
