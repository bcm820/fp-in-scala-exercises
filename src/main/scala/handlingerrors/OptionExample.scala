// Using std lib Option
// map.get returns an Option
object OptionExample {

  case class Employee(name: String, department: String) {
    def manager: Option[Employee] = managers.get(department)
  }

  def lookup(name: String): Option[Employee] = employees.get(name)

  val sally = Employee("Sally", "Dairy")
  val bob = Employee("Bob", "Dairy")
  val susie = Employee("Susie", "Poultry")

  val managers = Map("Dairy" -> sally)
  val employees = Map(
    "Sally" -> sally,
    "Bob" -> bob,
    "Susie" -> susie
  )

  lookup("Sammy") // None
  lookup("Sally") // Some(Employee(Sally,Poultry))
  lookup("Bob").map(_.department) // Some("Dairy")
  lookup("Bob").flatMap(_.manager) // Some(Employee(Sally,Dairy))
  lookup("Susie").flatMap(_.manager) // None
  lookup("Sammy").map(_.department) // None
  lookup("Sammy").flatMap(_.manager) // None (!)

  lookup("Bob")
    .map(_.department)
    .filter(_ != "Dairy")
    .getOrElse("NotDairy") // NotDairy

  lookup("Bob")
    .flatMap(_.manager)
    .filter(_ != sally)
    .getOrElse(susie) // Employee(Susie,Poultry)

}
