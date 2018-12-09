package applicatives

import java.text.SimpleDateFormat
import java.util.Date

trait Validation[+E, +A]
case class Success[A](a: A) extends Validation[Nothing, A]
case class Failure[E](es: List[E]) extends Validation[E, Nothing]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Validations extends App {

  def validationAp[E] = new Applicative[({ type f[x] = Validation[E, x] })#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)
    def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
        f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b))     => Success(f(a, b))
        case (Failure(es1), Failure(es2)) => Failure(es1 ++ es2)
        case (Failure(es), _)             => Failure(es)
        case (_, Failure(es))             => Failure(es)
      }
  }

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure(List("Name cannot be empty"))

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try { Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate)) } catch {
      case _: Throwable =>
        Failure(List("Birthdate must be in the form yyyy-MM-dd"))
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure(List("Phone number must be 10 digits"))

  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =
    validationAp.map3(validName(name),
                      validBirthdate(birthdate),
                      validPhone(phone))(WebForm(_, _, _))

  validWebForm("Brian", "2018-12-08", "5555555555")
  // Success(WebForm(
  // Brian,
  // Sat Dec 08 00:00:00 EST 2018,
  // 5555555555
  // ))

  validWebForm("", "1", "1")
  // Failure(List(
  // Name cannot be empty,
  // Birthdate must be in the form yyyy-MM-dd,
  // Phone number must be 10 digits
  // ))

}
