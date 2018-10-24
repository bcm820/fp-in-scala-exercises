package whatisfp

import util.Random

class CreditCard(last: Int) {
  def charge(price: Double): Double = price
  override def toString = s"#${last.toString}"
}

object CreditCard {
  def apply(): CreditCard =
    new CreditCard((new Random).nextInt((9999 - 1000) + 1))
}

case class Coffee(price: Double = 2.0)

case class Charge(cc: CreditCard, amt: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc) Charge(cc, amt + other.amt)
    else throw new Exception("Can't combine charges from different cards")
}
