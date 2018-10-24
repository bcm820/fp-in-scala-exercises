package whatisfp

object ImpureVsPure extends App {
  val cafe = new Cafe
  val cc = CreditCard()
  cafe.buyAndChargeForCoffee(cc)
  cafe.buyCoffee(cc)
  val cc2 = CreditCard()
  cafe.coalesce(List(Charge(cc, 2.0), Charge(cc, 1.5), Charge(cc2, 2.0)))
}
