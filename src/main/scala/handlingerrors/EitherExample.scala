package handlingerrors

object EitherExample extends App {

  case class Person(name: Name, age: Age)
  case class Name(value: String)
  case class Age(value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("empty name")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("negative age")
    else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  // 4.8
  // In mkPerson, map2 is only able to report one error,
  // even if both the name and the age are invalid.
  // Write a function that can return a list with both errors.
  def mkPerson2(name: String, age: Int): Either[List[String], Person] =
    (mkName(name), mkAge(age)) match {
      case (Left(s1), Left(s2)) => Left(List(s1, s2))
      case (Left(s), Right(_))  => Left(List(s))
      case (Right(_), Left(s))  => Left(List(s))
      case (Right(n), Right(a)) => Right(Person(n, a))
    }

  mkPerson2("", -1) // Left(List("empty name", "negative age"))
  mkPerson2("", 2) // Left(List("empty name"))
  mkPerson2("Jim", -1) // Left(List("negative age"))
  mkPerson2("Jim", 2) // Right(Person(Name("Jim"), Age(2)))

}
