package effects

object Effects {

  // A simple IO type can be expressed as a Monoid.
  trait IO { self =>
    def run: Unit
    def zero = new IO { def run = () }
    def op(io: IO) = new IO { def run = self.run; io.run }
  }
  object IO {
    def unit[A](a: => A) = new IO { def run = a }
    def apply[A](a: => A): IO = unit(a)
  }

  def PrintLine(msg: String) = IO { println(msg) }

  case class Player(name: String, score: Int)

  // Given some side effect (e.g. println), keep code as pure as possible
  // by relegating the side effect to the outermost layer of the program.
  // Given an impure function, f: A => B, split it into two functions:
  // 1. Pure- A => D: Returns some Description of the result of f
  // 2. Impure- D => B: An interpreter of the description

  // Computes the outcome of a contest
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p2.score) Some(p2)
    else None

  // Returns a message given the outcome
  def winnerMsg(op: Option[Player]): String =
    op map {
      case Player(name, _) => s"$name wins!"
    } getOrElse "It's a draw."

  // Prints the message to the console
  def contest(p1: Player, p2: Player): Unit =
    PrintLine(winnerMsg(winner(p1, p2)))

  // Contest still produces an effect, but only its interpreter
  // (the `run` method of PrintLine) has the side effect.

}

object Effects_2 extends App {

  // To express an IO type that allows an input and yields a value
  // we can use a type parameter, and extend our API with map and flatMap
  // to use IO in for-comprehensions. Note IO now forms a Monad.
  trait IO[A] { self =>
    def run: A
    def ++[B](io: IO[B]) = new IO[B] { def run = { self.run; io.run; } }
    def map[B](f: A => B) = new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]) = new IO[B] { def run = f(self.run).run }
  }
  object IO {
    def unit[A](a: => A) = new IO[A] { def run = a }
    def apply[A](a: => A): IO[A] = unit(a)
  }

  // An example of an effect resulting of an input value is readLine,
  // which we can wrap in our IO type. Let's also rewrite our PrintLine fn.
  def ReadLine = IO[String] { readLine }
  def PrintLine(msg: String) = IO[Unit] { println(msg) }

  // A simple example: Fahrenheit to Celcius converter (usage: converter.run)
  def fahrenheitToCelcius(d: Double) = (d - 32) * 5.0 / 9.0
  def converter: IO[Unit] =
    for {
      _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelcius(d).toString)
    } yield ()

}
