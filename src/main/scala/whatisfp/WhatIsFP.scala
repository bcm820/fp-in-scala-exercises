package whatisfp

/*
 * Referential transparency and function purity
 * An expression `e` is referentially transparent if, for all programs `p`,
 * all occurrences of `e` in `p` can be replaced by the result of evaluating `e`
 * without affecting the meaning of `p`. A function `f` is pure if the expression
 * `f(x)` is referentially transparent for all referentially transparent `x`.
 */

class Cafe {

  /*
   * Simple example with side effects
   * Receives a credit card, charges it, and returns a coffee.
   * Difficult to test since calling the function has side effect
   * (e.g. authorizing and charging a credit card transaction).
   * Ideally, the charge logic should be separate from the credit card.
   */
  def buyAndChargeForCoffee(cc: CreditCard): Coffee = {
    val cup = Coffee()
    cc.charge(cup.price)
    cup
  }

  /*
   * A functional solution: Remove side effects
   * Receives a credit card and returns a new Charge as a value with the coffee.
   * Concerns of processing the transaction will be handled elsewhere (in Charge).
   * Charge is now a first-class value unknown to CreditCard.
   */
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    (cup, Charge(cc, cup.price))
  }

  /*
   * Implements the purchase of `n` cups of coffee.
   * List.fill(n)(x) creates a list with `n` copies of `x`.
   * Unzip splits the purchases into two separate lists (coffees and charges).
   * Returns a list of coffees and the combined total charged for all coffees.
   */
  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce(_.combine(_)))
  }

  /*
   * Example of building out the business logic for working with charges.
   * Cafe can now coalesce same-card charges and save on processing fees.
   */
  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_.combine(_))).toList

}

object WhatIsFP extends App {
  val cafe = new Cafe
  val cc = CreditCard()
  cafe.buyAndChargeForCoffee(cc)
  cafe.buyCoffee(cc)
  val cc2 = CreditCard()
  cafe.coalesce(List(Charge(cc, 2.0), Charge(cc, 1.5), Charge(cc2, 2.0)))
}
